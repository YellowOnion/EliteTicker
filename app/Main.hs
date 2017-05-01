{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Control.Monad

import Pipes.ZMQ4
import Pipes.Zlib

import qualified System.ZMQ4 as Z

import qualified Data.ByteString as B
import qualified Data.HashMap.Strict as Hm

import Data.Aeson
import qualified Data.List as L

import Data.Maybe (isJust)
import Data.Monoid ((<>))

import qualified Data.Text as T
import qualified Data.Text.IO as T

import System.IO

import EDDN.Types

receiveMessages :: MonadIO m => Z.Socket Z.Sub -> Producer B.ByteString m ()
receiveMessages s = forever (receiveMessage s) >-> concatBS

printPrice :: T.Text -> EddnMessage -> IO ()
printPrice theName msg =
  case commods of
    [] -> return ()
    (x:_) -> T.putStrLn $ price x <> " cr, price of " <> theName <> " at " <> location
  where
    msg' = message msg
    location = (stationName msg') <> ", " <> (systemName msg')
    price = T.pack . show . sellPrice
    commods = filter ((==theName) . name) . commodities $ msg'

printBS :: MonadIO m => Consumer B.ByteString m ()
printBS = forever $ do
  bs <- await
  case eitherDecodeStrict bs of
    Right eddn -> liftIO $ (printPrice "Tea" eddn)
    Left _ -> return ()

concatBS :: MonadIO m => Pipe [B.ByteString] B.ByteString m ()
concatBS = forever $ do
  items <- await
  case items of
    [] -> liftIO . putStrLn $ "IDK Why this happens."
    allitems -> yield (B.concat allitems)

main :: IO ()
main = Z.withContext $ \ctx ->
       Z.withSocket ctx Z.Sub $ \soc -> do
  Z.subscribe soc (B.pack [])
  Z.connect soc "tcp://eddn-relay.elite-markets.net:9500"
  runSafeT . runEffect $ for (receiveMessages soc) (\x -> decompress defaultWindowBits (yield x)) >-> printBS
