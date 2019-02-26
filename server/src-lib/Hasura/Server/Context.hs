module Hasura.Server.Context
  ( HResponse(..)
  , Header (..)
  , Headers
  )
  where

import           Hasura.Prelude

import qualified Data.ByteString.Lazy as BL

newtype Header
  = Header { unHeader :: (Text, Text) }
  deriving (Show, Eq)

type Headers = [Header]

data HResponse
  = HResponse
  { _hrBody    :: !BL.ByteString
  , _hrHeaders :: !(Maybe Headers)
  } deriving (Show, Eq)
