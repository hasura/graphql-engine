module Hasura.RQL.DDL.Relationship.Types where

import           Hasura.Prelude
import           Hasura.RQL.Types
import           Hasura.SQL.Types

import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import qualified Data.HashMap.Strict        as HM
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import           Instances.TH.Lift          ()
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

data ObjRelManualConfig
  = ObjRelManualConfig
  { _ormTable          :: !QualifiedTable
  , _ormColumns        :: !(M.Map PGCol PGCol)
  , _ormInsertionOrder :: !RelInsertOrd
  } deriving (Show, Eq, Lift)

instance FromJSON ObjRelManualConfig where
  parseJSON (Object v) =
    ObjRelManualConfig
    <$> v .:  "remote_table"
    <*> v .:  "column_mapping"
    <*> v .:! "insertion_order" .!= RIOBeforeParent

  parseJSON _ =
    fail "manual_configuration should be an object"

instance ToJSON ObjRelManualConfig where
  toJSON (ObjRelManualConfig qt cm rio) =
    object [ "remote_table" .= qt
           , "column_mapping" .= cm
           , "insertion_order" .= rio
           ]

data ArrRelManualConfig
  = ArrRelManualConfig
  { _armTable   :: !QualifiedTable
  , _armColumns :: !(M.Map PGCol PGCol)
  } deriving (Show, Eq, Lift)

instance FromJSON ArrRelManualConfig where
  parseJSON (Object v) =
    ArrRelManualConfig
    <$> v .:  "remote_table"
    <*> v .:  "column_mapping"

  parseJSON _ =
    fail "manual_configuration should be an object"

instance ToJSON ArrRelManualConfig where
  toJSON (ArrRelManualConfig qt cm ) =
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

data ArrRelUsingFKeyOn
  = ArrRelUsingFKeyOn
  { arufTable  :: !QualifiedTable
  , arufColumn :: !PGCol
  } deriving (Show, Eq, Lift)

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''ArrRelUsingFKeyOn)

type ArrRelUsing = RelUsing ArrRelUsingFKeyOn ArrRelManualConfig
type ArrRelDef = RelDef ArrRelUsing
type CreateArrRel = WithTable ArrRelDef

data ObjRelUsingFKeyOn
  = ORUFColumn !PGCol
  | ORUFRemoteTable !QualifiedTable !PGCol
  deriving (Show, Eq, Lift)

instance FromJSON ObjRelUsingFKeyOn where
  parseJSON v@(String _) = ORUFColumn <$> parseJSON v
  parseJSON (Object o) = ORUFRemoteTable
                         <$> o .: "table"
                         <*> o .: "column"
  parseJSON _ = fail "expecting either String or Object"

instance ToJSON ObjRelUsingFKeyOn where
  toJSON (ORUFColumn column) = toJSON column
  toJSON (ORUFRemoteTable tableName column) =
    object [ "table" .= tableName
           , "column" .= column
           ]

type ObjRelUsing = RelUsing ObjRelUsingFKeyOn ObjRelManualConfig
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
