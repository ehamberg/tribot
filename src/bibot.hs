{-# LANGUAGE OverloadedStrings #-}

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import Control.Monad (when)
import Data.List (find)

mkRandSentence = "foo"

storeSentence m = return ()

onMessage :: EventFunc
onMessage s m
  | nick `B.isInfixOf` msg = sendMsg s chan mkRandSentence
  | otherwise = storeSentence msg
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = B.pack (cNick conf)

events :: [IrcEvent]
events = [Privmsg onMessage]

conf :: IrcConfig
conf = defaultConfig
  { cAddr = "ipv6.chat.freenode.net" -- Address
  , cNick = "bibot"                  -- Nickname
  , cChannels = ["#bibot"]           -- Channels to join on connect
  , cEvents = events                 -- Events to bind
  }

mkTable :: DB.Connection -> IO ()
mkTable db = do
  let q1 = "CREATE TABLE bigram(w1 VARCHAR(64), w2 VARCHAR(64), count INTEGER, "
         ++ "PRIMARY KEY (w1,w2))"
  let q2 = "CREATE TABLE startwords(word VARCHAR(64), count INTEGER, "
         ++ "PRIMARY KEY (word))"
  let q3 = "CREATE TABLE endwords(word VARCHAR(64), count INTEGER, "
         ++ "PRIMARY KEY (word))"
  putStrLn "creating database tables:"
  putStrLn "\tcreating table \"bigram\"..."
  DB.run db q1 []
  putStrLn "\tcreating table \"startwords\"..."
  DB.run db q2 []
  putStrLn "\tcreating table \"endwords\"..."
  DB.run db q3 []
  DB.commit db

addBigram :: DB.Connection -> B.ByteString -> B.ByteString -> IO ()
addBigram db word1 word2 = do
  DB.run db (insert w1' w2') []
  DB.run db (update w1' w2') []
  DB.commit db
    where w1' = B.unpack word1
          w2' = B.unpack word2
          insert w1 w2 = "INSERT OR IGNORE INTO bigram (w1, w2, count)"
                       ++ " VALUES (\"" ++ w1 ++ "\", \"" ++ w2 ++ "\", 0);"
          update w1 w2 = " UPDATE bigram set count = count+1 where w1=\""
                       ++ w1 ++ "\" and w2=\"" ++ w2 ++ "\"";

main :: IO ()
main = do
  db <- DB.connectSqlite3 "bigrams.db"
  tables <- DB.getTables db
  when ((isNothing . find (=="bigram")) tables) (mkTable db)
  addBigram db "hei" "du"
  addBigram db "æøå" "lol"
  connect conf False True
  DB.disconnect db
