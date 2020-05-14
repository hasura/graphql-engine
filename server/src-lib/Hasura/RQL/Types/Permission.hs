module Hasura.RQL.Types.Permission
  ( PermType(..)
  , permTypeToCode
  , PermId(..)
  ) where

import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.Session
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Hashable
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import qualified PostgreSQL.Binary.Decoding as PD

data PermType
  = PTInsert
  | PTSelect
  | PTUpdate
  | PTDelete
  deriving (Eq, Lift, Generic)
instance NFData PermType
instance Cacheable PermType

instance Q.FromCol PermType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "insert" -> Just PTInsert
    "update" -> Just PTUpdate
    "select" -> Just PTSelect
    "delete" -> Just PTDelete
    _   -> Nothing

permTypeToCode :: PermType -> T.Text
permTypeToCode PTInsert = "insert"
permTypeToCode PTSelect = "select"
permTypeToCode PTUpdate = "update"
permTypeToCode PTDelete = "delete"

instance Hashable PermType where
  hashWithSalt salt a = hashWithSalt salt $ permTypeToCode a

instance Show PermType where
  show PTInsert = "insert"
  show PTSelect = "select"
  show PTUpdate = "update"
  show PTDelete = "delete"

instance FromJSON PermType where
  parseJSON (String "insert") = return PTInsert
  parseJSON (String "select") = return PTSelect
  parseJSON (String "update") = return PTUpdate
  parseJSON (String "delete") = return PTDelete
  parseJSON _ =
    fail "perm_type should be one of 'insert', 'select', 'update', 'delete'"

instance ToJSON PermType where
  toJSON = String . permTypeToCode

data PermId
  = PermId
  { pidTable :: !TableName
  , pidRole  :: !RoleName
  , pidType  :: !PermType
  } deriving (Eq)

instance Show PermId where
  show (PermId tn rn pType) =
    show $ mconcat
    [ getTableTxt tn
    , "."
    , roleNameToTxt rn
    , "."
    , T.pack $ show pType
    ]
