module Hasura.RQL.Types.Relationship where

import           Hasura.Incremental       (Cacheable)
import           Hasura.Prelude

import           Control.Lens             (makeLenses)
import           Data.Aeson.TH
import           Data.Aeson.Types
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend

import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T

data RelDef a
  = RelDef
  { _rdName    :: !RelName
  , _rdUsing   :: !a
  , _rdComment :: !(Maybe T.Text)
  } deriving (Show, Eq, Generic)
instance (Cacheable a) => Cacheable (RelDef a)
$(deriveFromJSON hasuraJSON{omitNothingFields=True} ''RelDef)
$(makeLenses ''RelDef)

instance (ToJSON a) => ToJSON (RelDef a) where
  toJSON = object . toAesonPairs

instance (ToJSON a) => ToAesonPairs (RelDef a) where
 toAesonPairs (RelDef rn ru rc) =
  [ "name" .= rn
  , "using" .= ru
  , "comment" .= rc
  ]

data RelManualConfig (b :: BackendType)
  = RelManualConfig
  { rmTable   :: !(TableName b)
  , rmColumns :: !(HashMap (Column b) (Column b))
  } deriving (Generic)
deriving instance Backend b => Eq (RelManualConfig b)
deriving instance Backend b => Show (RelManualConfig b)
instance (Backend b) => Cacheable (RelManualConfig b)

instance (Backend b) => FromJSON (RelManualConfig b) where
  parseJSON (Object v) =
    RelManualConfig
    <$> v .:  "remote_table"
    <*> v .:  "column_mapping"

  parseJSON _ =
    fail "manual_configuration should be an object"

instance (Backend b) => ToJSON (RelManualConfig b) where
  toJSON (RelManualConfig qt cm) =
    object [ "remote_table" .= qt
           , "column_mapping" .= cm
           ]

data RelUsing (b :: BackendType) a
  = RUFKeyOn !a
  | RUManual !(RelManualConfig b)
  deriving (Show, Eq, Generic)
instance (Backend b, Cacheable a) => Cacheable (RelUsing b a)

instance (Backend b, ToJSON a) => ToJSON (RelUsing b a) where
  toJSON (RUFKeyOn fkey) =
    object [ "foreign_key_constraint_on" .= fkey ]
  toJSON (RUManual manual) =
    object [ "manual_configuration" .= manual ]

instance (FromJSON a, Backend b) => FromJSON (RelUsing b a) where
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

data ArrRelUsingFKeyOn (b :: BackendType)
  = ArrRelUsingFKeyOn
  { arufTable  :: !(TableName b)
  , arufColumn :: !(Column b)
  } deriving (Generic)
deriving instance Backend b => Eq (ArrRelUsingFKeyOn b)
deriving instance Backend b => Show (ArrRelUsingFKeyOn b)
instance Backend b => Cacheable (ArrRelUsingFKeyOn b)

instance (Backend b) => FromJSON (ArrRelUsingFKeyOn b) where
  parseJSON = genericParseJSON hasuraJSON{omitNothingFields=True}

instance (Backend b) => ToJSON (ArrRelUsingFKeyOn b) where
  toJSON = genericToJSON hasuraJSON{omitNothingFields=True}

-- TODO: This has to move to a common module
data WithTable b a
  = WithTable
  { wtSource :: !SourceName
  , wtName   :: !(TableName b)
  , wtInfo   :: !a
  }
deriving instance (Backend b, Show a) => Show (WithTable b a)
deriving instance (Backend b, Eq a) => Eq (WithTable b a)

instance (FromJSON a, Backend b) => FromJSON (WithTable b a) where
  parseJSON v@(Object o) =
    WithTable
    <$> o .:? "source" .!= defaultSource
    <*> o .: "table"
    <*> parseJSON v
  parseJSON _ =
    fail "expecting an Object with key 'table'"

instance (ToAesonPairs a, Backend b) => ToJSON (WithTable b a) where
  toJSON (WithTable sourceName tn rel) =
    object $ ("source" .= sourceName):("table" .= tn):toAesonPairs rel


type ArrRelUsing b = RelUsing b (ArrRelUsingFKeyOn b)
type ArrRelDef b = RelDef (ArrRelUsing b)
type CreateArrRel b = WithTable b (ArrRelDef b)

type ObjRelUsing b = RelUsing b (Column b)
type ObjRelDef b = RelDef (ObjRelUsing b)
type CreateObjRel b = WithTable b (ObjRelDef b)

data DropRel
  = DropRel
  { drSource       :: !SourceName
  , drTable        :: !(TableName 'Postgres)
  , drRelationship :: !RelName
  , drCascade      :: !Bool
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON{omitNothingFields=True} ''DropRel)

instance FromJSON DropRel where
  parseJSON = withObject "Object" $ \o ->
    DropRel
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "relationship"
      <*> o .:? "cascade" .!= False

data SetRelComment
  = SetRelComment
  { arSource       :: !SourceName
  , arTable        :: !(TableName 'Postgres)
  , arRelationship :: !RelName
  , arComment      :: !(Maybe T.Text)
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON{omitNothingFields=True} ''SetRelComment)
instance FromJSON SetRelComment where
  parseJSON = withObject "Object" $ \o ->
    SetRelComment
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "relationship"
      <*> o .:? "comment"

data RenameRel
  = RenameRel
  { rrSource  :: !SourceName
  , rrTable   :: !(TableName 'Postgres)
  , rrName    :: !RelName
  , rrNewName :: !RelName
  } deriving (Show, Eq)
$(deriveToJSON hasuraJSON ''RenameRel)

instance FromJSON RenameRel where
  parseJSON = withObject "Object" $ \o ->
    RenameRel
      <$> o .:? "source" .!= defaultSource
      <*> o .: "table"
      <*> o .: "name"
      <*> o .: "new_name"

-- should this be parameterized by both the source and the destination backend?
data RelInfo (b :: BackendType)
  = RelInfo
  { riName       :: !RelName
  , riType       :: !RelType
  , riMapping    :: !(HashMap (Column b) (Column b))
  , riRTable     :: !(TableName b)
  , riIsManual   :: !Bool
  , riIsNullable :: !Bool
  } deriving (Generic)
deriving instance Backend b => Show (RelInfo b)
deriving instance Backend b => Eq   (RelInfo b)
instance Backend b => NFData (RelInfo b)
instance Backend b => Cacheable (RelInfo b)
instance Backend b => Hashable (RelInfo b)

instance (Backend b) => FromJSON (RelInfo b) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => ToJSON (RelInfo b) where
  toJSON = genericToJSON hasuraJSON

fromRel :: RelName -> FieldName
fromRel = FieldName . relNameToTxt
