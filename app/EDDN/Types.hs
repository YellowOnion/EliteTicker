{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module EDDN.Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics


data EddnMessage = EddnMessage {
    schemaRef :: T.Text
  , header :: Header
  , message :: Message
  } deriving Show

instance FromJSON EddnMessage where
  parseJSON = withObject "EddnMessage" $ \v -> EddnMessage
                      <$> v .: "$schemaRef"
                      <*> v .: "header"
                      <*> v .: "message"

data Header = Header {
    uploaderID :: T.Text
  , softwareName :: T.Text
  , softwareVersion :: T.Text
  , gatewayTimestamp :: Maybe T.Text
  } deriving (Show, Generic)
instance FromJSON Header

data Message = Message {
    commodities :: [Commodity]
  , systemName :: T.Text
  , stationName :: T.Text
  , timestamp :: T.Text
  } deriving (Show, Generic)
instance FromJSON Message

data Commodity = Commodity {
    name :: T.Text
  , meanPrice :: Int
  , buyPrice :: Int
  , sellPrice :: Int
  , stock :: Int
  , stockBracket :: Int
  , demand :: Int
  , demandBracket :: Int
  , statusFlag :: Maybe [T.Text]
  } deriving (Show, Generic)
instance FromJSON Commodity

