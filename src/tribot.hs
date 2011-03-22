{-# LANGUAGE OverloadedStrings #-}

import System.Random (getStdRandom, randomR)
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.ByteString.UTF8 (fromString, toString)
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import Control.Monad.State
import Data.List (find)
import Data.Char (isSpace)
import Data.String hiding (fromString)
import Data.ByteString.Search
import Data.ByteString.Lazy (toChunks)

botIrcName :: String
botIrcName     = "tribot"
botIrcServer :: String
botIrcServer   = "ipv6.chat.freenode.net"
botIrcChannels :: [String]
botIrcChannels = ["#tribot"]

-- this function takes the message and the nick that sent it and returns true
-- if the message should be ignored
ignore :: B.ByteString -> B.ByteString -> Bool
ignore msg nick = any (==True)
                [ "http://" `B.isInfixOf` msg   -- ignore urls
                , "https://" `B.isInfixOf` msg
                , "bot" `B.isSuffixOf` nick     -- ignore bots
                , "@" `B.isPrefixOf` msg        -- ignore lambdabot commands
                , ">" `B.isPrefixOf` msg
                , nick == "lambdabot"           -- ignore lambdabot’s ramblings
                ]

trigrams :: (IsString a) => [a] -> [[a]]
trigrams [_] = []
trigrams ws = trigrams' (["<s>"] ++ ws ++ ["<e>"])
  where trigrams' xs
          | length xs < 3 = []
          | otherwise     = take 3 xs : trigrams' (tail xs)

-- returns true if the given two words have been observed at the end of an
-- utterance
areEndWords :: DB.Connection -> [B.ByteString] -> IO Bool
areEndWords db ws = do
  n <- liftM length $ DB.quickQuery' db
                     "SELECT * FROM trigram WHERE w1=? AND w2=? AND w3='<e>'"
                     (map DB.toSql ws)
  return (n/=0)

-- returns true if the given two words have a next word in our trigram model
hasNext :: DB.Connection -> [B.ByteString] -> IO Bool
hasNext db ws = do
  n <- liftM length $ DB.quickQuery' db
                     "SELECT * FROM trigram WHERE w1=? AND w2=? AND w3<>'<e>'"
                     (map DB.toSql ws)
  return (n/=0)

pick :: [(B.ByteString, Int)] -> IO B.ByteString
pick ws = do
  let freqSum = sum $ map snd ws
  rand <- getStdRandom (randomR (1,freqSum))
  return $ pickElem ws rand
    where pickElem ~((a,b):r) n = if b >= n then a else pickElem r (n-b)


getStartWord :: DB.Connection -> IO B.ByteString
getStartWord db = do
  candidates <- DB.quickQuery' db
               "SELECT w2,count FROM trigram WHERE w1='<s>'"
               []
  pick (map conv candidates)


-- makes a random sentence by finding a random start word and then finding
-- random words to follow the sentence as it's built
randSentence :: DB.Connection -> StateT [B.ByteString] IO B.ByteString
randSentence db = do
  current <- get
  let n = length current

  -- if empty, find a start word
  when (null current) $ liftIO (getStartWord db) >>= \s -> put ["<s>",s]

  -- get the last two words of the sentence under construction
  lastTwo <- fmap (drop (n-2)) get

  candidates <- liftIO $ DB.quickQuery' db
    "SELECT w3,count FROM trigram WHERE w1=? AND w2=? AND w3<>'<e>'"
    (map DB.toSql lastTwo)

  if null candidates
     then returnCurrent
     else do end <- liftIO $ areEndWords db lastTwo
             hasN <-liftIO $  hasNext db lastTwo

             -- if there is no next word: stop. also, if this is an end word,
             -- stop with a probability that is equal to the sentence's length
             -- (in per cent) even if there are more words
             len <- liftM length get
             stop <- liftM (<=len) $ liftIO $ getStdRandom (randomR (1,100))
             if not hasN || (stop && end)
                then returnCurrent
                else do next <- liftIO $ pick (map conv candidates)
                        modify (++[next])
                        randSentence db
    where returnCurrent = fmap (B.intercalate " " . tail) get

-- convert database rows to (bytestring, int) tuples
conv :: [DB.SqlValue] -> (B.ByteString, Int)
conv ~[a,b] = (DB.fromSql a::B.ByteString, DB.fromSql b::Int)

-- stores a sentence in the given database. the trigrams are put in the
-- “trigram” table and the first and last word in “startword“ and “endword”,
-- respectively.
storeSentence :: DB.Connection -> B.ByteString -> IO ()
storeSentence db s = do
  -- split into tokens
  let tokens = filter (`notElem` ["", "<s>", "<e>"]) $ B.splitWith isSpace s
  -- if first token ends with ‘:’, assume it's a nick and replace it with “<n>:”
  let tokens' = if (not . null) tokens && (B.last . head) tokens == ':'
                 then "<n>:":tail tokens
                 else tokens
  -- store trigrams
  mapM_ (\[w1,w2,w3] -> addTrigram db w1 w2 w3) $ trigrams tokens'
  DB.commit db

-- adds a trigram to the database
addTrigram :: DB.Connection -> B.ByteString -> B.ByteString -> B.ByteString -> IO ()
addTrigram db w1 w2 w3 = do
  DB.run db "INSERT OR IGNORE INTO trigram VALUES (?, ?, ?, 0)" ws'
  DB.run db "UPDATE trigram set count=count+1 where w1=? AND w2=? AND w3=?" ws'
  DB.commit db
    where ws' = map (DB.toSql . toString) [w1, w2, w3]

-- replace str with repl in given string
replace' :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
replace' str repl string = (B.concat . toChunks) $ replace str repl string

onMessage :: DB.Connection -> EventFunc
onMessage db s m
  -- if message should be ignored, do nothing
  | ignore msg from = return ()
  -- ignore private queries
  | chan == nick = sendMsg s from $ fromString "♫ Lalala. I'm ignoring you. ♬"
  -- if the bot’s nick is mentioned, store what was said and generate and send
  -- a sentence to the channel
  | nick `B.isInfixOf` msg = do
      sentence <- liftM (replace' from "<n>") $ evalStateT (randSentence db) []
      sendMsg s chan sentence

      -- replace the bot's nick with “<n>” and store the sentenc that triggered
      -- the response
      storeSentence db (replace' nick "<n>" msg)
  -- store the sentence
  | otherwise = storeSentence db msg
    where chan = fromJust $ mChan m
          msg  = mMsg m
          nick = B.pack botIrcName
          from = fromMaybe "" (mNick m)

-- set up the initial database tables
mkTable :: DB.Connection -> IO ()
mkTable db = do
  let q1 = "CREATE TABLE trigram(w1 TEXT, w2 TEXT, w3 TEXT, count INTEGER, "
         ++ "PRIMARY KEY (w1,w2,w3))"
  putStrLn "Creating database table \"trigram\"..."
  DB.run db q1 []
  DB.run db "INSERT INTO trigram VALUES('<s>','hi!','<e>', 1)" []
  DB.commit db
  return ()

conf :: DB.Connection -> IrcConfig
conf db = defaultConfig
         { cAddr        = botIrcServer        -- Address
         , cNick        = botIrcName          -- Nickname
         , cChannels    = botIrcChannels      -- Channels to join
         , cUsername    = "tribot"
         , cRealname    = "tribot"
         , cCTCPVersion = "tribot"
         , cEvents      = [Privmsg (onMessage db)]
         }

main :: IO ()
main = do
  db <- DB.connectSqlite3 "trigrams.db"
  tables <- DB.getTables db
  when ((isNothing . find (=="trigram")) tables) (mkTable db)
  connect (conf db) False True
  DB.disconnect db
