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
    RelTarget (..),
    RelInfo (..),
    RelManualTableConfig (..),
    RelManualNativeQueryConfig (..),
    RelManualCommon (..),
    RelUsing (..),
    WithTable (..),
    boolToNullable,
    fromRel,
    rdComment,
    rdName,
    rdUsing,
  )
where

import Autodocodec (HasCodec (codec), HasObjectCodec, dimapCodec, disjointEitherCodec, optionalField', requiredField')
import Autodocodec qualified as AC
import Autodocodec.Extended (optionalFieldOrIncludedNull', typeableName)
import Control.Lens (makeLenses)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.Text qualified as T
import Data.Typeable (Typeable)
import Hasura.NativeQuery.Types (NativeQueryName)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Common

data RelDef a = RelDef
  { _rdName :: RelName,
    _rdUsing :: a,
    _rdComment :: Maybe T.Text
  }
  deriving (Show, Eq, Generic)

instance (HasCodec a, Typeable a) => HasCodec (RelDef a) where
  codec = AC.object ("RelDef_" <> typeableName @a) AC.objectCodec

instance (HasCodec a) => HasObjectCodec (RelDef a) where
  objectCodec =
    RelDef
      <$> requiredField' "name"
      AC..= _rdName
        <*> requiredField' "using"
      AC..= _rdUsing
        <*> optionalField' "comment"
      AC..= _rdComment

instance (FromJSON a) => FromJSON (RelDef a) where
  parseJSON = genericParseJSON hasuraJSON {omitNothingFields = True}

$(makeLenses ''RelDef)

instance (ToJSON a) => ToJSON (RelDef a) where
  toJSON = object . toAesonPairs

instance (ToJSON a) => ToAesonPairs (RelDef a) where
  toAesonPairs (RelDef rn ru rc) =
    [ "name" .= rn,
      "using" .= ru,
      "comment" .= rc
    ]

data RelManualTableConfig (b :: BackendType) = RelManualTableConfig
  { rmtTable :: TableName b,
    rmtCommon :: RelManualCommon b
  }
  deriving (Generic)
  deriving (FromJSON, ToJSON) via AC.Autodocodec (RelManualTableConfig b)

deriving instance (Backend b) => Eq (RelManualTableConfig b)

deriving instance (Backend b) => Show (RelManualTableConfig b)

data RelManualNativeQueryConfig (b :: BackendType) = RelManualNativeQueryConfig
  { rmnNativeQueryName :: NativeQueryName,
    rmnCommon :: RelManualCommon b
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (RelManualNativeQueryConfig b)

deriving instance (Backend b) => Show (RelManualNativeQueryConfig b)

data RelManualCommon (b :: BackendType) = RelManualCommon
  { rmColumns :: HashMap (Column b) (Column b),
    rmInsertOrder :: Maybe InsertOrder
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (RelManualCommon b)

deriving instance (Backend b) => Show (RelManualCommon b)

instance (Backend b) => HasCodec (RelManualTableConfig b) where
  codec =
    AC.object (backendPrefix @b <> "RelManualTableConfig")
      $ RelManualTableConfig
      <$> requiredField' "remote_table"
      AC..= rmtTable
        <*> AC.objectCodec
      AC..= rmtCommon

instance (Backend b) => HasCodec (RelManualNativeQueryConfig b) where
  codec =
    AC.object (backendPrefix @b <> "RelManualNativeQueryConfig")
      $ RelManualNativeQueryConfig
      <$> requiredField' "remote_native_query"
      AC..= rmnNativeQueryName
        <*> AC.objectCodec
      AC..= rmnCommon

instance (Backend b) => AC.HasObjectCodec (RelManualCommon b) where
  objectCodec =
    RelManualCommon
      <$> requiredField' "column_mapping"
      AC..= rmColumns
        <*> optionalFieldOrIncludedNull' "insertion_order"
      AC..= rmInsertOrder

data RelUsing (b :: BackendType) a
  = RUFKeyOn a
  | RUManual (RelManualTableConfig b)
  deriving (Show, Eq, Generic)

instance (Backend b, HasCodec a, Typeable a) => HasCodec (RelUsing b a) where
  codec = dimapCodec dec enc $ disjointEitherCodec fkCodec manualCodec
    where
      fkCodec =
        AC.object ("RUFKeyOn_" <> typeableName @a)
          $ requiredField' "foreign_key_constraint_on"

      manualCodec =
        AC.object (backendPrefix @b <> "RUManual")
          $ requiredField' "manual_configuration"

      dec = either RUFKeyOn RUManual
      enc (RUFKeyOn fkey) = Left fkey
      enc (RUManual manual) = Right manual

instance (Backend b, ToJSON a) => ToJSON (RelUsing b a) where
  toJSON (RUFKeyOn fkey) =
    object ["foreign_key_constraint_on" .= fkey]
  toJSON (RUManual manual) =
    object ["manual_configuration" .= manual]

instance (FromJSON a, Backend b) => FromJSON (RelUsing b a) where
  parseJSON (Object o) = do
    let fkeyOnM = KM.lookup "foreign_key_constraint_on" o
        manualM = KM.lookup "manual_configuration" o
        msgFrag = "one of foreign_key_constraint_on/manual_configuration should be present"
    case (fkeyOnM, manualM) of
      (Nothing, Nothing) -> fail $ "atleast " <> msgFrag
      (Just a, Nothing) -> RUFKeyOn <$> parseJSON a
      (Nothing, Just b) -> RUManual <$> parseJSON b
      _ -> fail $ "only " <> msgFrag
  parseJSON _ =
    fail "using should be an object"

data ArrRelUsingFKeyOn (b :: BackendType) = ArrRelUsingFKeyOn
  { arufTable :: TableName b,
    arufColumns :: NonEmpty (Column b)
  }
  deriving (Generic)

deriving instance (Backend b) => Eq (ArrRelUsingFKeyOn b)

deriving instance (Backend b) => Show (ArrRelUsingFKeyOn b)

-- TODO: This has to move to a common module
data WithTable b a = WithTable
  { wtSource :: SourceName,
    wtName :: TableName b,
    wtInfo :: a
  }

deriving instance (Backend b, Show a) => Show (WithTable b a)

deriving instance (Backend b, Eq a) => Eq (WithTable b a)

instance (FromJSON a, Backend b) => FromJSON (WithTable b a) where
  parseJSON v@(Object o) =
    WithTable
      <$> o
      .:? "source"
      .!= defaultSource
      <*> o
      .: "table"
      <*> parseJSON v
  parseJSON _ =
    fail "expecting an Object with key 'table'"

instance (ToAesonPairs a, Backend b) => ToJSON (WithTable b a) where
  toJSON (WithTable sourceName tn rel) =
    object $ ("source" .= sourceName) : ("table" .= tn) : toAesonPairs rel

data ObjRelUsingChoice b
  = SameTable (NonEmpty (Column b))
  | RemoteTable (TableName b) (NonEmpty (Column b))
  deriving (Generic)

deriving instance (Backend b) => Eq (ObjRelUsingChoice b)

deriving instance (Backend b) => Show (ObjRelUsingChoice b)

instance (Backend b) => HasCodec (ObjRelUsingChoice b) where
  codec = dimapCodec dec enc $ disjointEitherCodec sameTableCodec remoteTableCodec
    where
      sameTableCodec :: AC.JSONCodec (Either (Column b) (NonEmpty (Column b)))
      sameTableCodec = disjointEitherCodec codec codec

      remoteTableCodec :: AC.JSONCodec (Either (TableName b, Column b) (TableName b, NonEmpty (Column b)))
      remoteTableCodec =
        singleOrMultipleRelColumnsCodec @b
          $ backendPrefix @b
          <> "ObjRelRemoteTable"

      dec = \case
        Left (Left col) -> SameTable $ pure col
        Left (Right cols) -> SameTable $ cols
        Right (Left (qt, col)) -> RemoteTable qt $ pure col
        Right (Right (qt, cols)) -> RemoteTable qt $ cols

      enc = \case
        SameTable (col :| []) -> Left $ Left col
        SameTable cols -> Left $ Right cols
        RemoteTable qt (col :| []) -> Right $ Left (qt, col)
        RemoteTable qt cols -> Right $ Right (qt, cols)

singleOrMultipleRelColumnsCodec ::
  forall b.
  (Backend b) =>
  Text ->
  AC.JSONCodec
    ( Either
        (TableName b, Column b)
        (TableName b, NonEmpty (Column b))
    )
singleOrMultipleRelColumnsCodec codecName =
  disjointEitherCodec
    ( AC.object (codecName <> "SingleColumn")
        $ (,)
        <$> requiredField' "table"
        AC..= fst
          <*> requiredField' "column"
        AC..= snd
    )
    ( AC.object (codecName <> "MultipleColumns")
        $ (,)
        <$> requiredField' "table"
        AC..= fst
          <*> requiredField' "columns"
        AC..= snd
    )

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

instance (Backend b) => HasCodec (ArrRelUsingFKeyOn b) where
  codec =
    dimapCodec dec enc
      $ singleOrMultipleRelColumnsCodec @b (backendPrefix @b <> "ArrRelUsingFKeyOn")
    where
      dec :: (Either (TableName b, Column b) (TableName b, NonEmpty (Column b))) -> ArrRelUsingFKeyOn b
      dec = \case
        Left (qt, col) -> ArrRelUsingFKeyOn qt (pure col)
        Right (qt, cols) -> ArrRelUsingFKeyOn qt cols

      enc :: ArrRelUsingFKeyOn b -> (Either (TableName b, Column b) (TableName b, NonEmpty (Column b)))
      enc = \case
        ArrRelUsingFKeyOn qt (col :| []) -> Left (qt, col)
        ArrRelUsingFKeyOn qt cols -> Right (qt, cols)

instance (Backend b) => ToJSON (ArrRelUsingFKeyOn b) where
  toJSON ArrRelUsingFKeyOn {arufTable = _arufTable, arufColumns = _arufColumns} =
    object
      $ ("table" .= _arufTable)
      : case _arufColumns of
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

---

data RelTarget b
  = RelTargetTable (TableName b)
  | RelTargetNativeQuery NativeQueryName
  deriving (Generic)

deriving instance (Backend b) => Eq (RelTarget b)

deriving instance (Backend b) => Ord (RelTarget b)

deriving instance (Backend b) => Show (RelTarget b)

instance (Backend b) => NFData (RelTarget b)

instance (Backend b) => Hashable (RelTarget b)

instance (Backend b) => FromJSON (RelTarget b) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => ToJSON (RelTarget b) where
  toJSON = genericToJSON hasuraJSON

---

data RelInfo (b :: BackendType) = RelInfo
  { riName :: RelName,
    riType :: RelType,
    riMapping :: HashMap (Column b) (Column b),
    riTarget :: RelTarget b,
    riIsManual :: Bool,
    riInsertOrder :: InsertOrder
  }
  deriving (Generic)

deriving instance (Backend b) => Show (RelInfo b)

deriving instance (Backend b) => Eq (RelInfo b)

deriving instance (Backend b) => Ord (RelInfo b)

instance (Backend b) => NFData (RelInfo b)

instance (Backend b) => Hashable (RelInfo b)

instance (Backend b) => FromJSON (RelInfo b) where
  parseJSON = genericParseJSON hasuraJSON

instance (Backend b) => ToJSON (RelInfo b) where
  toJSON = genericToJSON hasuraJSON

data Nullable = Nullable | NotNullable
  deriving (Eq, Show, Generic)

instance NFData Nullable

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
