{-# LANGUAGE OverloadedStrings #-}

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import Control.Monad (when)
import Data.List (find)

botIrcName     = "bibot"
botIrcServer   = "ipv6.chat.freenode.net"
botIrcChannels = ["#bitbot"]

bigrams :: [a] -> [[a]]
bigrams [x] = []
bigrams xs  = take 2 xs : bigrams (tail xs)

mkRandSentence = "foo"

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

onMessage :: EventFunc
onMessage s m
  | nick `B.isInfixOf` msg = sendMsg s chan msg
  | otherwise = storeSentence undefined msg
  where chan = fromJust $ mChan m
        msg = mMsg m
        nick = B.pack botIrcName

mkTable :: DB.Connection -> IO ()
mkTable db = do
  let q1 = "CREATE TABLE bigram(w1 VARCHAR(64), w2 VARCHAR(64), count INTEGER, "
         ++ "PRIMARY KEY (w1,w2))"
  let q2 = "CREATE TABLE startword(word VARCHAR(64), count INTEGER, "
         ++ "PRIMARY KEY (word))"
  let q3 = "CREATE TABLE endword(word VARCHAR(64), count INTEGER, "
         ++ "PRIMARY KEY (word))"
  putStrLn "creating database tables:"
  putStrLn "\tcreating table \"bigram\"..."
  DB.run db q1 []
  putStrLn "\tcreating table \"startword\"..."
  DB.run db q2 []
  putStrLn "\tcreating table \"endword\"..."
  DB.run db q3 []
  DB.commit db

conf = defaultConfig
         { cAddr     = botIrcServer        -- Address
         , cNick     = botIrcName          -- Nickname
         , cChannels = botIrcChannels      -- Channels to join
         , cEvents   = [Privmsg onMessage]
         }

main :: IO ()
main = do
  db <- DB.connectSqlite3 "bigrams.db"
  tables <- DB.getTables db
  when ((isNothing . find (=="bigram")) tables) (mkTable db)
  connect conf False True
  DB.disconnect db
