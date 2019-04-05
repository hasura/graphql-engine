module Hasura.Server.PGDump where

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Hasura.Prelude

data PGDumpReqBody
  = PGDumpReqBody
  { prbSchema :: !String
  , prgOpts   :: !(Maybe String)
  }
  deriving (Show, Eq)

$(deriveJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''PGDumpReqBody)
