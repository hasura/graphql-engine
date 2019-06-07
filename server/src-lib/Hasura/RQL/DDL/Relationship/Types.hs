module Hasura.RQL.DDL.Relationship.Types where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Instances.TH.Lift ()
import           Language.Haskell.TH.Syntax (Lift)

data RelDef a
  = RelDef
  { rdName    :: !RelName
  , rdUsing   :: !a
  , rdComment :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)

$(deriveFromJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''RelDef)

instance (ToJSON a) => ToJSON (RelDef a) where
  toJSON = object . toAesonPairs

instance (ToJSON a) => ToAesonPairs (RelDef a) where
 toAesonPairs (RelDef rn ru rc) =
  [ "name" .= rn
  , "using" .= ru
  , "comment" .= rc
  ]

data RelManualConfig
  = RelManualConfig
  { rmTable   :: !QualifiedTable
  , rmColumns :: !(M.Map PGCol PGCol)
  } deriving (Show, Eq, Lift)

instance FromJSON RelManualConfig where
  parseJSON (Object v) =
    RelManualConfig
    <$> v .:  "remote_table"
    <*> v .:  "column_mapping"

  parseJSON _ =
    fail "manual_configuration should be an object"

instance ToJSON RelManualConfig where
  toJSON (RelManualConfig qt cm) =
    object [ "remote_table" .= qt
           , "column_mapping" .= cm
           ]

data RelUsing a b
  = RUFKeyOn a
  | RUManual b
  deriving (Show, Eq, Lift)

instance (ToJSON a, ToJSON b) => ToJSON (RelUsing a b) where
  toJSON (RUFKeyOn fkey) =
    object [ "foreign_key_constraint_on" .= fkey ]
  toJSON (RUManual manual) =
    object [ "manual_configuration" .= manual ]

instance (FromJSON a, FromJSON b) => FromJSON (RelUsing a b) where
  parseJSON (Object o) = do
    let fkeyOnM = HM.lookup "foreign_key_constraint_on" o
        manualM = HM.lookup "manual_configuration" o
    let msgFrag = "one of foreign_key_constraint_on/manual_configuration should be present"
    case (fkeyOnM, manualM) of
      (Nothing, Nothing) -> fail $ "atleast " <> msgFrag
      (Just a, Nothing)  -> RUFKeyOn <$> parseJSON a
      (Nothing, Just b)  -> RUManual <$> parseJSON b
      _                  -> fail $ "only " <> msgFrag
  parseJSON _ =
    fail "using should be an object"

newtype ArrRelManualConfig =
  ArrRelManualConfig { getArrRelMapping :: RelManualConfig }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

data ArrRelUsingFKeyOn
  = ArrRelUsingFKeyOn
  { arufTable  :: !QualifiedTable
  , arufColumn :: !PGCol
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''ArrRelUsingFKeyOn)

type ArrRelUsing = RelUsing ArrRelUsingFKeyOn ArrRelManualConfig
type ArrRelDef = RelDef ArrRelUsing
type CreateArrRel = WithTable ArrRelDef

newtype ObjRelManualConfig =
  ObjRelManualConfig { getObjRelMapping :: RelManualConfig }
  deriving (Show, Eq, FromJSON, ToJSON, Lift)

type ObjRelUsing = RelUsing PGCol ObjRelManualConfig
type ObjRelDef = RelDef ObjRelUsing

type CreateObjRel = WithTable ObjRelDef

data DropRel
  = DropRel
  { drTable        :: !QualifiedTable
  , drRelationship :: !RelName
  , drCascade      :: !(Maybe Bool)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DropRel)

data SetRelComment
  = SetRelComment
  { arTable        :: !QualifiedTable
  , arRelationship :: !RelName
  , arComment      :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''SetRelComment)

data RenameRel
  = RenameRel
  { rrTable   :: !QualifiedTable
  , rrName    :: !RelName
  , rrNewName :: !RelName
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 2 snakeCase) ''RenameRel)
