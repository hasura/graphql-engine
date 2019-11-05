module Hasura.Server.Context
  ( HttpResponse(..)
  , Header (..)
  , Headers
  , ETagConfig(..)
  )
  where

import           Hasura.Prelude

newtype Header
  = Header { unHeader :: (Text, Text) }
  deriving (Show, Eq)

type Headers = [Header]

data ETagConfig
  = ETCUseETag
  | ETCDonotUseETag

data HttpResponse a
  = HttpResponse
  { _hrBody    :: !a
  , _hrHeaders :: !(Maybe Headers)
  , _hrUseETag :: !ETagConfig
  }
