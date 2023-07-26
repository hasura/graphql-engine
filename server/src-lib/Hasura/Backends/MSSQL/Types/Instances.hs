{- ORMOLU_DISABLE -}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
-- NOTE: This module previously used Template Haskell to generate its instances,
-- but additional restrictions on Template Haskell splices introduced in GHC 9.0 impose an ordering
-- on the generated instances that is difficult to satisfy
-- To avoid these difficulties, we now use CPP.

-- | MSSQL Types Instances
--
-- Instances for types from "Hasura.Backends.MSSQL.Types.Internal" that're slow to compile.
module Hasura.Backends.MSSQL.Types.Instances () where

import Autodocodec (HasCodec (codec), dimapCodec, optionalFieldWithDefault', parseAlternative, requiredField')
import qualified Autodocodec as AC
import Data.Aeson.Extended
import Data.Aeson.Types
import Data.Text.Extended (ToTxt (..))
import Data.Time as Time
import Hasura.Backends.MSSQL.Types.Internal
import Hasura.Base.ErrorValue qualified as ErrorValue
import Hasura.Metadata.DTO.Placeholder (placeholderCodecViaJSON)
import Hasura.Prelude
import Language.Haskell.TH.Syntax
import Hasura.Base.ToErrorValue
import Hasura.RQL.Types.Backend qualified as Backend
import Hasura.RQL.Types.BackendType qualified as Backend
import qualified Hasura.RQL.Types.Backend as IR

deriving instance Generic (Aliased a)
instance Hashable a => Hashable (Aliased a)
deriving instance Eq a => Eq (Aliased a)
instance NFData a => NFData (Aliased a)
deriving instance Show a => Show (Aliased a)
deriving instance Functor Aliased
deriving instance Data a => Data (Aliased a)


#define INSTANCE_CLUMP_1(name) \
         deriving instance Generic name ;\
         instance Hashable name ;\
         deriving instance Eq name ;\
         deriving instance Show name ;\
         deriving instance Data name ;\
         instance NFData name ;\
         instance FromJSON name ;\
         deriving instance Ord name
INSTANCE_CLUMP_1(UnifiedTableName)
INSTANCE_CLUMP_1(UnifiedObjectRelationship)
INSTANCE_CLUMP_1(UnifiedArrayRelationship)
INSTANCE_CLUMP_1(UnifiedUsing)
INSTANCE_CLUMP_1(UnifiedOn)
INSTANCE_CLUMP_1(UnifiedColumn)
INSTANCE_CLUMP_1(TempTableName)
INSTANCE_CLUMP_1(SomeTableName)
INSTANCE_CLUMP_1(ConstraintName)


#define INSTANCE_CLUMP_2(name) \
         deriving instance Generic name ;\
         instance Hashable name ;\
         deriving instance Eq name ;\
         deriving instance Show name ;\
         deriving instance Data name ;\
         instance NFData name
INSTANCE_CLUMP_2(Where)
INSTANCE_CLUMP_2(For)
INSTANCE_CLUMP_2(Aggregate)
INSTANCE_CLUMP_2(EntityAlias)
INSTANCE_CLUMP_2(ForJson)
INSTANCE_CLUMP_2(JsonCardinality)
INSTANCE_CLUMP_2(Root)
INSTANCE_CLUMP_2(OrderBy)
INSTANCE_CLUMP_2(JoinAlias)
INSTANCE_CLUMP_2(Reselect)
INSTANCE_CLUMP_2(ColumnName)
INSTANCE_CLUMP_2(DataLength)
INSTANCE_CLUMP_2(Expression)
INSTANCE_CLUMP_2(FunctionApplicationExpression)
INSTANCE_CLUMP_2(MethodApplicationExpression)
INSTANCE_CLUMP_2(NullsOrder)
INSTANCE_CLUMP_2(Order)
INSTANCE_CLUMP_2(ScalarType)
INSTANCE_CLUMP_2(TableName)
INSTANCE_CLUMP_2(FunctionName)
INSTANCE_CLUMP_2(Select)
INSTANCE_CLUMP_2(With)
INSTANCE_CLUMP_2(CTEBody)
INSTANCE_CLUMP_2(Top)
INSTANCE_CLUMP_2(FieldName)
INSTANCE_CLUMP_2(JsonPath)
INSTANCE_CLUMP_2(Op)
INSTANCE_CLUMP_2(SpatialOp)
INSTANCE_CLUMP_2(Projection)
INSTANCE_CLUMP_2(From)
INSTANCE_CLUMP_2(OpenJson)
INSTANCE_CLUMP_2(JsonFieldSpec)
INSTANCE_CLUMP_2(Join)
INSTANCE_CLUMP_2(JoinSource)
INSTANCE_CLUMP_2(SelectIntoTempTable)
INSTANCE_CLUMP_2(SITTConstraints)
INSTANCE_CLUMP_2(InsertValuesIntoTempTable)
INSTANCE_CLUMP_2(InsertOutput)
INSTANCE_CLUMP_2(Inserted)
INSTANCE_CLUMP_2(OutputColumn)
INSTANCE_CLUMP_2(TempTable)
INSTANCE_CLUMP_2(Deleted)
INSTANCE_CLUMP_2(DeleteOutput)
INSTANCE_CLUMP_2(Values)
INSTANCE_CLUMP_2(Delete)
INSTANCE_CLUMP_2(Insert)
INSTANCE_CLUMP_2(Merge)
INSTANCE_CLUMP_2(MergeUsing)
INSTANCE_CLUMP_2(MergeOn)
INSTANCE_CLUMP_2(MergeWhenMatched)
INSTANCE_CLUMP_2(MergeWhenNotMatched)

deriving instance Ord TableName
deriving instance Ord FunctionName
deriving instance Ord ScalarType

deriving instance Lift TableName
deriving instance Lift FunctionName
deriving instance Lift NullsOrder
deriving instance Lift Order

--------------------------------------------------------------------------------
-- Third-party types

deriving instance Generic (Time.TimeZone)

--------------------------------------------------------------------------------
-- Debug instances

instance ToErrorValue ScalarType where
  toErrorValue = ErrorValue.squote . tshow

instance ToErrorValue TableName where
  toErrorValue = ErrorValue.squote . tshow

instance ToErrorValue ConstraintName where
  toErrorValue = ErrorValue.squote . constraintNameText

instance ToErrorValue ColumnName where
  toErrorValue = ErrorValue.squote . columnNameText

instance ToErrorValue FunctionName where
  toErrorValue = ErrorValue.squote . tshow

instance ToTxt ScalarType where
  toTxt = tshow -- TODO: include schema

instance ToTxt TableName where
  toTxt (TableName tableName (SchemaName tableSchema)) =
    if tableSchema == "dbo"
      then tableName
      else tableSchema <> "." <> tableName

instance ToTxt FunctionName where
  toTxt (FunctionName functionName (SchemaName functionSchema)) =
    if functionSchema == "dbo"
      then functionName
      else functionSchema <> "." <> functionName

instance ToTxt ColumnName where
  toTxt = columnNameText

instance ToTxt ConstraintName where
  toTxt = constraintNameText

#define INSTANCE_CLUMP_3(name) \
         instance ToJSON name where \
           { toJSON = genericToJSON hasuraJSON } ;\
         instance FromJSON name where \
           { parseJSON = genericParseJSON hasuraJSON }
INSTANCE_CLUMP_3(Order)
INSTANCE_CLUMP_3(NullsOrder)
INSTANCE_CLUMP_3(FieldName)

instance ToJSON ScalarType where
  toJSON scalarType = String $ scalarTypeDBName DataLengthUnspecified scalarType

instance FromJSON ScalarType where
  parseJSON (String s) = pure (parseScalarType s)
  parseJSON _ = fail "expected a string"

instance HasCodec ColumnName where
  codec = dimapCodec ColumnName columnNameText codec

deriving instance FromJSON ColumnName

deriving instance ToJSON ColumnName

deriving instance ToJSON ConstraintName

instance ToJSON FunctionName where
  toJSON = genericToJSON hasuraJSON

instance HasCodec FunctionName where
  codec = parseAlternative objCodec strCodec
    where
      objCodec =
        AC.object "MSSQLFunctionName" $
          FunctionName
            <$> requiredField' "name" AC..= functionName
            <*> optionalFieldWithDefault' "schema" "dbo" AC..= functionSchema
      strCodec = flip FunctionName "dbo" <$> codec

instance FromJSON FunctionName where
  parseJSON v@(String _) =
    FunctionName <$> parseJSON v <*> pure "dbo"
  parseJSON (Object o) =
    FunctionName
      <$> o .: "name"
      <*> o .:? "schema" .!= "dbo"
  parseJSON _ =
    fail "expecting a string/object for FunctionName"

instance ToJSONKey FunctionName where
  toJSONKey = toJSONKeyText $ \(FunctionName name (SchemaName schema)) -> schema <> "." <> name

instance HasCodec TableName where
  codec = parseAlternative objCodec strCodec
    where
      objCodec =
        AC.object "MSSQLTableName" $
          TableName
            <$> requiredField' "name" AC..= tableName
            <*> optionalFieldWithDefault' "schema" "dbo" AC..= tableSchema
      strCodec = flip TableName "dbo" <$> codec

instance HasCodec SchemaName where
  codec = dimapCodec SchemaName _unSchemaName codec

instance FromJSON TableName where
  parseJSON v@(String _) =
    TableName <$> parseJSON v <*> pure "dbo"
  parseJSON (Object o) =
    TableName
      <$> o .: "name"
      <*> o .:? "schema" .!= "dbo"
  parseJSON _ =
    fail "expecting a string/object for TableName"

instance ToJSON TableName where
  toJSON = genericToJSON hasuraJSON

instance ToJSONKey TableName where
  toJSONKey = toJSONKeyText $ \(TableName name (SchemaName schema)) -> schema <> "." <> name

instance ToJSONKey ScalarType

-- NOTE!: an empty (default) instance declaration here caused a bug; instead
-- use standalone deriving via underlying Int instance
deriving newtype instance ToJSONKey ColumnName

deriving newtype instance FromJSONKey ColumnName

--------------------------------------------------------------------------------
-- Manual instances

deriving instance Generic (CountType n)

deriving instance (Backend.Backend 'Backend.MSSQL, Show n,
  Show (IR.AggregationPredicates 'Backend.MSSQL n),
  Show (IR.FunctionArgumentExp 'Backend.MSSQL n),
  Show (IR.BooleanOperators 'Backend.MSSQL n)) => Show (CountType n)
deriving instance (Backend.Backend 'Backend.MSSQL) => Functor CountType
deriving instance (Backend.Backend 'Backend.MSSQL) => Foldable CountType
deriving instance (Backend.Backend 'Backend.MSSQL) => Traversable CountType

deriving instance Foldable Countable
deriving instance Traversable Countable

deriving instance (Backend.Backend 'Backend.MSSQL, Eq n,
  Eq (IR.AggregationPredicates 'Backend.MSSQL n),
  Eq (IR.FunctionArgumentExp 'Backend.MSSQL n),
  Eq (IR.BooleanOperators 'Backend.MSSQL n)) => Eq (CountType n)

deriving instance Generic (Countable n)

instance Hashable n => Hashable (Countable n)

deriving instance Eq n => Eq (Countable n)

deriving instance Show n => Show (Countable n)

deriving instance Data n => Data (Countable n)

instance NFData n => NFData (Countable n)

instance ToJSON n => ToJSON (Countable n)

instance FromJSON n => FromJSON (Countable n)

deriving instance Ord ColumnName

deriving instance Monoid Where

deriving instance Semigroup Where

instance Monoid Top where
  mempty = NoTop

instance Semigroup Top where
  (<>) :: Top -> Top -> Top
  (<>) NoTop x = x
  (<>) x NoTop = x
  (<>) (Top x) (Top y) = Top (min x y)

deriving instance Generic (BooleanOperators a)

deriving instance Functor BooleanOperators

deriving instance Foldable BooleanOperators

deriving instance Traversable BooleanOperators

deriving instance Show a => Show (BooleanOperators a)

deriving instance Eq a => Eq (BooleanOperators a)

instance NFData a => NFData (BooleanOperators a)

instance Hashable a => Hashable (BooleanOperators a)

instance ToJSON a => ToJSONKeyValue (BooleanOperators a) where
  toJSONKeyValue = \case
    ASTContains a -> ("_st_contains", toJSON a)
    ASTCrosses a -> ("_st_crosses", toJSON a)
    ASTEquals a -> ("_st_equals", toJSON a)
    ASTIntersects a -> ("_st_intersects", toJSON a)
    ASTOverlaps a -> ("_st_overlaps", toJSON a)
    ASTTouches a -> ("_st_touches", toJSON a)
    ASTWithin a -> ("_st_within", toJSON a)

instance HasCodec ScalarType where
  codec = AC.named "ScalarType" placeholderCodecViaJSON
