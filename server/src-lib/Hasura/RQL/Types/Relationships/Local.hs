{-# LANGUAGE TemplateHaskell #-}

module Hasura.RQL.Types.Relationships.Local
  ( ArrRelDef,
    ArrRelUsing,
    ArrRelUsingFKeyOn (..),
    Nullable (..),
    ObjRelDef,
    ObjRelUsing,
    ObjRelUsingChoice (..),
    RelDef (..),
    RelInfo (..),
    RelManualConfig (..),
    RelUsing (..),
    WithTable (..),
    boolToNullable,
    fromRel,
    rdComment,
    rdName,
    rdUsing,
  )
where

import Control.Lens (makeLenses)
import Data.Aeson.TH
import Data.Aeson.Types
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Common
import Hasura.SQL.Backend

data RelDef a = RelDef
  { _rdName :: !RelName,
    _rdUsing :: !a,
    _rdComment :: !(Maybe T.Text)
  }
  deriving (Show, Eq, Generic)

instance (Cacheable a) => Cacheable (RelDef a)

$(deriveFromJSON hasuraJSON {omitNothingFields = True} ''RelDef)
$(makeLenses ''RelDef)

instance (ToJSON a) => ToJSON (RelDef a) where
  toJSON = object . toAesonPairs

instance (ToJSON a) => ToAesonPairs (RelDef a) where
  toAesonPairs (RelDef rn ru rc) =
    [ "name" .= rn,
      "using" .= ru,
      "comment" .= rc
    ]

data RelManualConfig (b :: BackendType) = RelManualConfig
  { rmTable :: !(TableName b),
    rmColumns :: !(HashMap (Column b) (Column b)),
    rmInsertOrder :: !(Maybe InsertOrder)
  }
  deriving (Generic)

deriving instance Backend b => Eq (RelManualConfig b)

deriving instance Backend b => Show (RelManualConfig b)

instance (Backend b) => Cacheable (RelManualConfig b)

instance (Backend b) => FromJSON (RelManualConfig b) where
  parseJSON (Object v) =
    RelManualConfig
      <$> v .: "remote_table"
      <*> v .: "column_mapping"
      <*> v .:? "insertion_order"
  parseJSON _ =
    fail "manual_configuration should be an object"

instance (Backend b) => ToJSON (RelManualConfig b) where
  toJSON (RelManualConfig qt cm io) =
    object
      [ "remote_table" .= qt,
        "column_mapping" .= cm,
        "insertion_order" .= io
      ]

data RelUsing (b :: BackendType) a
  = RUFKeyOn !a
  | RUManual !(RelManualConfig b)
  deriving (Show, Eq, Generic)

instance (Backend b, Cacheable a) => Cacheable (RelUsing b a)

instance (Backend b, ToJSON a) => ToJSON (RelUsing b a) where
  toJSON (RUFKeyOn fkey) =
    object ["foreign_key_constraint_on" .= fkey]
  toJSON (RUManual manual) =
    object ["manual_configuration" .= manual]

instance (FromJSON a, Backend b) => FromJSON (RelUsing b a) where
  parseJSON (Object o) = do
    let fkeyOnM = HM.lookup "foreign_key_constraint_on" o
        manualM = HM.lookup "manual_configuration" o
        msgFrag = "one of foreign_key_constraint_on/manual_configuration should be present"
    case (fkeyOnM, manualM) of
      (Nothing, Nothing) -> fail $ "atleast " <> msgFrag
      (Just a, Nothing) -> RUFKeyOn <$> parseJSON a
      (Nothing, Just b) -> RUManual <$> parseJSON b
      _ -> fail $ "only " <> msgFrag
  parseJSON _ =
    fail "using should be an object"

data ArrRelUsingFKeyOn (b :: BackendType) = ArrRelUsingFKeyOn
  { arufTable :: !(TableName b),
    arufColumns :: !(NonEmpty (Column b))
  }
  deriving (Generic)

deriving instance Backend b => Eq (ArrRelUsingFKeyOn b)

deriving instance Backend b => Show (ArrRelUsingFKeyOn b)

instance Backend b => Cacheable (ArrRelUsingFKeyOn b)

-- TODO: This has to move to a common module
data WithTable b a = WithTable
  { wtSource :: !SourceName,
    wtName :: !(TableName b),
    wtInfo :: !a
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
    object $ ("source" .= sourceName) : ("table" .= tn) : toAesonPairs rel

data ObjRelUsingChoice b
  = SameTable !(NonEmpty (Column b))
  | RemoteTable !(TableName b) !(NonEmpty (Column b))
  deriving (Generic)

deriving instance Backend b => Eq (ObjRelUsingChoice b)

deriving instance Backend b => Show (ObjRelUsingChoice b)

instance (Backend b) => Cacheable (ObjRelUsingChoice b)

instance (Backend b) => ToJSON (ObjRelUsingChoice b) where
  toJSON = \case
    SameTable (col :| []) -> toJSON col
    SameTable cols -> toJSON cols
    RemoteTable qt (lcol :| []) ->
      object
        [ "table" .= qt,
          "column" .= lcol
        ]
    RemoteTable qt lcols ->
      object
        [ "table" .= qt,
          "columns" .= lcols
        ]

instance (Backend b) => FromJSON (ObjRelUsingChoice b) where
  parseJSON = \case
    Object o -> do
      table <- o .: "table"
      column <- o .:? "column"
      columns <- o .:? "columns"
      cols <- case (column, columns) of
        (Just col, Nothing) -> parseColumns col
        (Nothing, Just cols) -> parseColumns cols
        _ -> fail "expected exactly one of 'column' or 'columns'"
      pure $ RemoteTable table cols
    val -> SameTable <$> parseColumns val
    where
      parseColumns :: Value -> Parser (NonEmpty (Column b))
      parseColumns = \case
        v@(String _) -> pure <$> parseJSON v
        v@(Array _) -> parseJSON v
        _ -> fail "Expected string or array"

instance (Backend b) => ToJSON (ArrRelUsingFKeyOn b) where
  toJSON ArrRelUsingFKeyOn {arufTable = _arufTable, arufColumns = _arufColumns} =
    object $
      ("table" .= _arufTable) :
      case _arufColumns of
        col :| [] -> ["column" .= col]
        cols -> ["columns" .= cols]

instance (Backend b) => FromJSON (ArrRelUsingFKeyOn b) where
  parseJSON = \case
    Object o -> do
      table <- o .: "table"
      column <- o .:? "column"
      columns <- o .:? "columns"
      cols <- case (column, columns) of
        (Just col, Nothing) -> parseColumns col
        (Nothing, Just cols) -> parseColumns cols
        _ -> fail "expected exactly one of 'column' or 'columns'"
      pure $ ArrRelUsingFKeyOn table cols
    _ -> fail "Expecting object { table, columns }."
    where
      parseColumns :: Value -> Parser (NonEmpty (Column b))
      parseColumns = \case
        v@(String _) -> pure <$> parseJSON v
        v@(Array _) -> parseJSON v
        _ -> fail "Expected string or array"

type ArrRelUsing b = RelUsing b (ArrRelUsingFKeyOn b)

type ArrRelDef b = RelDef (ArrRelUsing b)

type ObjRelUsing b = RelUsing b (ObjRelUsingChoice b)

type ObjRelDef b = RelDef (ObjRelUsing b)

data RelInfo (b :: BackendType) = RelInfo
  { riName :: !RelName,
    riType :: !RelType,
    riMapping :: !(HashMap (Column b) (Column b)),
    riRTable :: !(TableName b),
    riIsManual :: !Bool,
    riInsertOrder :: !InsertOrder
  }
  deriving (Generic)

deriving instance Backend b => Show (RelInfo b)

deriving instance Backend b => Eq (RelInfo b)

instance Backend b => NFData (RelInfo b)

instance Backend b => Cacheable (RelInfo b)

instance Backend b => Hashable (RelInfo b)

instance (Backend b) => FromJSON (RelInfo b) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => ToJSON (RelInfo b) where
  toJSON = genericToJSON hasuraJSON

data Nullable = Nullable | NotNullable
  deriving (Eq, Show, Generic)

instance NFData Nullable

instance Cacheable Nullable

instance Hashable Nullable

boolToNullable :: Bool -> Nullable
boolToNullable True = Nullable
boolToNullable False = NotNullable

instance FromJSON Nullable where
  parseJSON = fmap boolToNullable . parseJSON

instance ToJSON Nullable where
  toJSON =
    toJSON . \case
      Nullable -> True
      NotNullable -> False

fromRel :: RelName -> FieldName
fromRel = FieldName . relNameToTxt
