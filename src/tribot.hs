{-# LANGUAGE OverloadedStrings #-}

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import Data.ByteString.UTF8 (fromString)
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as DB
import Control.Monad.State
import Data.List (find)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import LanguageModel

-- this function takes the message and the nick that sent it and returns true
-- if the message should be ignored
ignore :: B.ByteString -> B.ByteString -> Bool
ignore msg nick = any (==True)
                [ "http://"  `B.isInfixOf`  msg  -- ignore urls
                , "https://" `B.isInfixOf`  msg
                , "bot"      `B.isSuffixOf` nick -- ignore bots
                , "@"        `B.isPrefixOf` msg  -- ignore lambdabot commands
                , ">"        `B.isPrefixOf` msg
                , "!"        `B.isPrefixOf` msg
                ]

onMessage :: B.ByteString -> DB.Connection -> EventFunc
onMessage nick db s m
  -- if message should be ignored, do nothing
  | ignore msg from = return ()
  -- ignore private queries
  | chan == nick = sendMsg s from $ fromString "♫ Lalala. I'm ignoring you. ♬"
  -- if the bot’s nick is mentioned, store what was said and generate and send
  -- a sentence to the channel
  | nick `B.isInfixOf` msg = do
      sentence <- liftM (replace' "<n>" from) $ evalStateT (randSentence db) []
      sendMsg s chan sentence

      -- replace the bot's nick with “<n>” and store the sentenc that triggered
      -- the response
      storeSentence db (replace' nick "<n>" msg)
  -- store the sentence
  | otherwise = storeSentence db msg
    where chan = fromJust $ mChan m
          msg  = mMsg m
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

conf :: String -> String -> [String] -> DB.Connection -> IrcConfig
conf nick server channels db = defaultConfig
         { cNick        = nick      -- Nickname
         , cAddr        = server    -- Server Address
         , cChannels    = channels  -- Channels to join
         , cUsername    = "tribot"
         , cRealname    = "tribot"
         , cCTCPVersion = "tribot"
         , cEvents      = [Privmsg (onMessage (B.pack nick) db)]
         }

main :: IO ()
main = do
  args <- getArgs
  when (length args < 3) $ do
    name <- getProgName
    putStrLn ("usage: " ++ name ++ " [nick] [server] [channel1] <channel2> ...")
    exitFailure
  let nick:server:channels = args
  db <- DB.connectSqlite3 "trigrams.db"
  tables <- DB.getTables db
  when ((isNothing . find (=="trigram")) tables) (mkTable db)
  connect (conf nick server channels db) False True
  DB.disconnect db
