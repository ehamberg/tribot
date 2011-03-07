{-# LANGUAGE OverloadedStrings #-}

import System.Random
import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import Control.Monad (when, liftM)
import Data.List (find)

botIrcName     = "bibot"
botIrcServer   = "ipv6.chat.freenode.net"
botIrcChannels = ["#bibot"]

bigrams :: [a] -> [[a]]
bigrams [x] = []
bigrams xs  = take 2 xs : bigrams (tail xs)

isEndWord :: DB.Connection -> B.ByteString -> IO Bool
isEndWord db word = do
  n <- liftM length $ DB.quickQuery' db
                     "SELECT * FROM endword WHERE word=?"
                     [DB.toSql word]
  return (n==0)

nextWords :: DB.Connection -> B.ByteString -> IO [B.ByteString]
nextWords db word = do
  candidates <- DB.quickQuery' db
               "SELECT w2, count FROM bigram WHERE w1=? ORDER BY count"
               [DB.toSql word]
  if candidates == []
     then return [word]
     else do let freqs = map conv candidates
             let freqSum = sum $ map snd freqs
             rand <- getStdRandom (randomR (1,freqSum))
             let next = pickElem freqs rand
             end <- isEndWord db next
             if end
                then return [next]
                else do xxx <- nextWords db next
                        return (next:xxx)

mkRandSentence db = do
  startWords <- DB.quickQuery' db
               "SELECT word, count FROM startword ORDER BY count"
               []
  let freqs = map conv startWords
  let freqSum = sum $ map snd freqs
  rand <- getStdRandom (randomR (1,freqSum))
  let startW = pickElem freqs rand
  s <- nextWords db startW
  return $ B.intercalate " " s

conv [a,b]    = (DB.fromSql a::B.ByteString, DB.fromSql b::Int)

pickElem ((a,b):r) n = if b >= n then a else pickElem r (n-b)

storeSentence :: DB.Connection -> B.ByteString -> IO ()
storeSentence db s = do
  DB.run db (insert "startword") [firstW]
  DB.run db (update "startword") [firstW]
  DB.run db (insert "endword") [lastW]
  DB.run db (update "endword") [lastW]
  mapM_ (\[w1,w2] -> addBigram db w1 w2) $ bigrams tokens
  DB.commit db
    where tokens   = filter (/="") $ B.split ' ' s
          firstW   = (DB.toSql . B.unpack . head) tokens -- first word
          lastW    = (DB.toSql . B.unpack . last) tokens -- last word
          insert t = "INSERT OR IGNORE INTO " ++ t ++ " VALUES (?, 0)"
          update t = "UPDATE " ++ t ++ " set count = count+1 where word=?";

addBigram :: DB.Connection -> B.ByteString -> B.ByteString -> IO ()
addBigram db word1 word2 = do
  DB.run db "INSERT OR IGNORE INTO bigram VALUES (?, ?, 0)" words
  DB.run db "UPDATE bigram set count = count+1 where w1=? and w2=?" words
  return ()
    where bToSql = DB.toSql . B.unpack
          words = [bToSql word1, bToSql word2]

onMessage :: DB.Connection -> EventFunc
onMessage db s m
  | nick `B.isInfixOf` msg = mkRandSentence db >>= sendMsg s chan
  | otherwise = storeSentence db msg
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = B.pack botIrcName

mkTable :: DB.Connection -> IO ()
mkTable db = do
  let q1 = "CREATE TABLE bigram(w1 TEXT, w2 TEXT, count INTEGER, "
         ++ "PRIMARY KEY (w1,w2))"
  let q2 = "CREATE TABLE startword(word TEXT, count INTEGER, "
         ++ "PRIMARY KEY (word))"
  let q3 = "CREATE TABLE endword(word TEXT, count INTEGER, "
         ++ "PRIMARY KEY (word))"
  putStrLn "creating database tables:"
  putStrLn "\tcreating table \"bigram\"..."
  DB.run db q1 []
  putStrLn "\tcreating table \"startword\"..."
  DB.run db q2 []
  putStrLn "\tcreating table \"endword\"..."
  DB.run db q3 []
  DB.commit db

conf db = defaultConfig
         { cAddr     = botIrcServer        -- Address
         , cNick     = botIrcName          -- Nickname
         , cChannels = botIrcChannels      -- Channels to join
         , cEvents   = [Privmsg (onMessage db)]
         }

main :: IO ()
main = do
  db <- DB.connectSqlite3 "bigrams.db"
  tables <- DB.getTables db
  when ((isNothing . find (=="bigram")) tables) (mkTable db)
  connect (conf db) False True
  DB.disconnect db
