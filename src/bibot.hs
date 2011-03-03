{-# LANGUAGE OverloadedStrings #-}

import Network.SimpleIRC
import Data.Maybe
import qualified Data.ByteString.Char8 as B

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

main :: IO (Either IOError MIrc)
main = connect conf False True
