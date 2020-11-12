module Hasura.RQL.Types.Relationship where

import           Hasura.Backends.Postgres.SQL.Types
import           Hasura.Incremental                 (Cacheable)
import           Hasura.Prelude

import           Control.Lens                       (makeLenses)
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Hasura.RQL.Types.Common
import           Instances.TH.Lift                  ()
import           Language.Haskell.TH.Syntax         (Lift)

import qualified Data.HashMap.Strict                as HM
import qualified Data.Text                          as T

data RelDef a
  = RelDef
  { _rdName    :: !RelName
  , _rdUsing   :: !a
  , _rdComment :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift, Generic)
instance (Cacheable a) => Cacheable (RelDef a)
$(deriveFromJSON (aesonDrop 3 snakeCase){omitNothingFields=True} ''RelDef)
$(makeLenses ''RelDef)

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
  , rmColumns :: !(HashMap PGCol PGCol)
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable RelManualConfig

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

data RelUsing a
  = RUFKeyOn !a
  | RUManual !RelManualConfig
  deriving (Show, Eq, Lift, Generic)
instance (Cacheable a) => Cacheable (RelUsing a)

instance (ToJSON a) => ToJSON (RelUsing a) where
  toJSON (RUFKeyOn fkey) =
    object [ "foreign_key_constraint_on" .= fkey ]
  toJSON (RUManual manual) =
    object [ "manual_configuration" .= manual ]

instance (FromJSON a) => FromJSON (RelUsing a) where
  parseJSON (Object o) = do
    let fkeyOnM = HM.lookup "foreign_key_constraint_on" o
        manualM = HM.lookup "manual_configuration" o
        msgFrag = "one of foreign_key_constraint_on/manual_configuration should be present"
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
  } deriving (Show, Eq, Lift, Generic)
instance Cacheable ArrRelUsingFKeyOn

$(deriveJSON (aesonDrop 4 snakeCase){omitNothingFields=True} ''ArrRelUsingFKeyOn)

type ArrRelUsing = RelUsing ArrRelUsingFKeyOn
type ArrRelDef = RelDef ArrRelUsing
type CreateArrRel = WithTable ArrRelDef

type ObjRelUsing = RelUsing PGCol
type ObjRelDef = RelDef ObjRelUsing
type CreateObjRel = WithTable ObjRelDef

data DropRel
  = DropRel
  { drTable        :: !QualifiedTable
  , drRelationship :: !RelName
  , drCascade      :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''DropRel)

instance FromJSON DropRel where
  parseJSON = withObject "Object" $ \o ->
    DropRel
      <$> o .: "table"
      <*> o .: "relationship"
      <*> o .:? "cascade" .!= False

data SetRelComment
  = SetRelComment
  { arTable        :: !QualifiedTable
  , arRelationship :: !RelName
  , arComment      :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 2 snakeCase){omitNothingFields=True} ''SetRelComment)
instance FromJSON SetRelComment where
  parseJSON = withObject "Object" $ \o ->
    SetRelComment
      <$> o .: "table"
      <*> o .: "relationship"
      <*> o .:? "comment"

data RenameRel
  = RenameRel
  { rrTable   :: !QualifiedTable
  , rrName    :: !RelName
  , rrNewName :: !RelName
  } deriving (Show, Eq, Lift)
$(deriveToJSON (aesonDrop 2 snakeCase) ''RenameRel)

instance FromJSON RenameRel where
  parseJSON = withObject "Object" $ \o ->
    RenameRel
      <$> o .: "table"
      <*> o .: "name"
      <*> o .: "new_name"
