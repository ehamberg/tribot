{-# LANGUAGE OverloadedStrings #-}

module LanguageModel where

import System.Random (getStdRandom, randomR)
import qualified Data.ByteString.Char8 as B
import Data.ByteString.UTF8 (toString)
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import Control.Monad.State
import Data.Char (isSpace)
import Data.ByteString.Search
import Data.ByteString.Lazy (toChunks)

trigrams :: [B.ByteString] -> [[B.ByteString]]
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

-- frequency-proportionate random selection of a ByteString
pick :: [(B.ByteString, Int)] -> IO B.ByteString
pick ws = do
  let freqSum = sum $ map snd ws
  rand <- getStdRandom (randomR (1,freqSum))
  return $ pickElem ws rand
    where pickElem ~((a,b):r) n = if b >= n then a else pickElem r (n-b)


-- select a random start word
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
