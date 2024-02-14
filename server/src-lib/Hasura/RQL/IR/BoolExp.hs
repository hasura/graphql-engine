{-# LANGUAGE UndecidableInstances #-}

-- | Boolean Expressions
--
-- This module defines the IR representation of boolean expressions
-- used in @_where@ clauses in GraphQL queries, permissions, and so on.
--
-- The types in this module define a /generic/ structure with "holes" to be filled
-- by each backend. Specifically, holes will include things like types for table names,
-- and backend field types.
module Hasura.RQL.IR.BoolExp
  ( BoolExp (..),
    ColExp (..),
    GBoolExp (..),
    gBoolExpTrue,
    GExists (..),
    DWithinGeomOp (..),
    DWithinGeogOp (..),
    CastExp,
    OpExpG (..),
    opExpDepCol,
    ComparisonNullability (..),
    STIntersectsNbandGeommin (..),
    STIntersectsGeomminNband (..),
    ComputedFieldBoolExp (..),
    AnnComputedFieldBoolExp (..),
    AnnBoolExpFld (..),
    RelationshipFilters (..),
    AnnBoolExp,
    AnnRedactionExpPartialSQL,
    AnnRedactionExpUnpreparedValue,
    AnnRedactionExp (..),
    annBoolExpTrue,
    andAnnBoolExps,
    AnnBoolExpFldSQL,
    AnnBoolExpSQL,
    PartialSQLExp (..),
    isStaticValue,
    hasStaticExp,
    AnnBoolExpPartialSQL,
    PreSetColsG,
    PreSetColsPartial,
    RootOrCurrentColumn (..),
    RootOrCurrent (..),
    RemoteRelPermBoolExp (..),
    RemoteRelRHSFetchInfo (..),
  )
where

import Autodocodec (Codec (CommentCodec), HasCodec (codec), JSONCodec, bimapCodec, dimapCodec, named, valueCodec)
import Control.DeepSeq (rnf)
import Control.Lens.Plated
import Data.Aeson.Extended
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (hashWithSalt)
import Data.Monoid
import Data.Text.Extended
import Hasura.Function.Cache
import Hasura.Prelude
import Hasura.RQL.IR.BoolExp.RemoteRelationshipPredicate (RemoteRelRHSFetchWhereExp)
import Hasura.RQL.IR.Value
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.BackendTag (backendPrefix)
import Hasura.RQL.Types.BackendType
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Relationships.Local
import Hasura.SQL.AnyBackend qualified as AB
import Hasura.Session

----------------------------------------------------------------------------------------------------
-- Boolean structure

-- | This type represents a boolean expression tree. It is parametric over the actual
-- implementation of the actual boolean term values. It nonetheless leaks some information:
-- "exists" is only used in permissions, to add conditions based on another table.
--
-- * The @backend@ parameter is used to find the backend-specific type for table names
--   in the @BoolExists@ constructor.
-- * The @field@ type represent the type of database-specific field types.
data GBoolExp (backend :: BackendType) field
  = BoolAnd [GBoolExp backend field]
  | BoolOr [GBoolExp backend field]
  | BoolNot (GBoolExp backend field)
  | -- | Represents a condition on an aribtrary table.
    -- since the @backend@ and @field@ are the same,
    -- the table must be of the same database type.
    BoolExists (GExists backend field)
  | -- | A column field
    BoolField field
  deriving (Show, Eq, Functor, Foldable, Traversable, Data, Generic)

instance (Backend b, NFData a) => NFData (GBoolExp b a)

instance (Backend b, Data a) => Plated (GBoolExp b a)

instance (Backend b, Hashable a) => Hashable (GBoolExp b a)

instance (Backend b, FromJSONKeyValue a) => FromJSON (GBoolExp b a) where
  parseJSON = withObject "boolean expression" \o ->
    BoolAnd <$> forM (KM.toList o) \(k, v) ->
      if
        | k == "$or" -> BoolOr <$> parseJSON v <?> Key k
        | k == "_or" -> BoolOr <$> parseJSON v <?> Key k
        | k == "$and" -> BoolAnd <$> parseJSON v <?> Key k
        | k == "_and" -> BoolAnd <$> parseJSON v <?> Key k
        | k == "$not" -> BoolNot <$> parseJSON v <?> Key k
        | k == "_not" -> BoolNot <$> parseJSON v <?> Key k
        | k == "$exists" -> BoolExists <$> parseJSON v <?> Key k
        | k == "_exists" -> BoolExists <$> parseJSON v <?> Key k
        | otherwise -> BoolField <$> parseJSONKeyValue (k, v)

instance (Backend backend, ToJSONKeyValue field) => ToJSON (GBoolExp backend field) where
  -- A representation for boolean values as JSON.
  toJSON be = case be of
    -- @and@ expressions can be represented differently than the rest
    -- if the keys are unique
    BoolAnd bExps ->
      let m = HashMap.fromList $ map getKV bExps
       in -- if the keys aren't repeated, then the special notation of object encoding can be used
          if length m == length bExps
            then toJSON m
            else object $ pure kv
    _ -> object $ pure kv
    where
      kv :: (Key, Value)
      kv = getKV be
      getKV :: GBoolExp backend field -> (Key, Value)
      getKV = \case
        BoolAnd bExps -> "_and" .= map toJSON bExps
        BoolOr bExps -> "_or" .= map toJSON bExps
        BoolNot bExp -> "_not" .= toJSON bExp
        BoolExists bExists -> "_exists" .= toJSON bExists
        BoolField a -> toJSONKeyValue a

-- | A default representation for a @true@ boolean value.
gBoolExpTrue :: GBoolExp backend field
gBoolExpTrue = BoolAnd []

-- | Represents a condition on an aribtrary table. Used as part of our permissions boolean
-- expressions. See our documentation for more information:
-- <https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#using-unrelated-tables-views>
data GExists (backend :: BackendType) field = GExists
  { _geTable :: TableName backend,
    _geWhere :: GBoolExp backend field
  }
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance (Backend b, Show a) => Show (GExists b a)

deriving instance (Backend b, Eq a) => Eq (GExists b a)

deriving instance (Backend b, Data a) => Data (GExists b a)

instance (Backend b, NFData a) => NFData (GExists b a)

instance (Backend b, Data a) => Plated (GExists b a)

instance (Backend b, Hashable a) => Hashable (GExists b a)

instance (Backend b, FromJSONKeyValue a) => FromJSON (GExists b a) where
  parseJSON = withObject "_exists" \o -> do
    qt <- o .: "_table"
    wh <- o .: "_where"
    pure $ GExists qt wh

instance (Backend b, ToJSONKeyValue a) => ToJSON (GExists b a) where
  toJSON (GExists gTable gWhere) =
    object
      [ "_table" .= gTable,
        "_where" .= gWhere
      ]

----------------------------------------------------------------------------------------------------
-- Boolean expressions in permissions

-- | We don't allow conditions across relationships in permissions: the type we use as the terms in
-- GBoolExp is this one, ColExp, which only contains a FieldName and a JSON Value.
data ColExp = ColExp
  { ceCol :: FieldName,
    ceVal :: Value
  }
  deriving (Show, Eq, Data, Generic)

instance NFData ColExp

instance FromJSONKeyValue ColExp where
  parseJSONKeyValue (k, v) = ColExp (FieldName (K.toText k)) <$> parseJSON v

instance ToJSONKeyValue ColExp where
  toJSONKeyValue (ColExp k v) = (K.fromText (getFieldNameTxt k), v)

-- | This @BoolExp@ type is a simple alias for the boolean expressions used in permissions, that
-- uses 'ColExp' as the term in GBoolExp.
newtype BoolExp (b :: BackendType) = BoolExp {unBoolExp :: GBoolExp b ColExp}
  deriving newtype (Show, Eq, Generic, NFData, ToJSON, FromJSON)

-- TODO: This implementation delegates to Aeson instances for encoding and
-- decoding GBoolExp. To accurately represent GBoolExp with a codec we will need
-- Autodocodec to gain support for expressing an object type with "additional
-- properties" for fields.
instance (Backend b) => HasCodec (BoolExp b) where
  codec = CommentCodec doc $ named (backendPrefix @b <> "BoolExp") $ dimapCodec BoolExp unBoolExp jsonCodec
    where
      jsonCodec :: JSONCodec (GBoolExp b ColExp)
      jsonCodec = bimapCodec (parseEither parseJSON) toJSON valueCodec
      doc =
        "Recursive object type with keys \"_and\", \"_or\", \"_not\", \"_exists\", or \"<field name>\". "
          <> "Values for \"_and\" and \"_or\" are arrays of nested expressions. "
          <> "A value for \"_not\" is a single nested expression. "
          <> "A value for \"_exists\" is an object with \"table\" and \"where\" properties where \"table\" is a table name, "
          <> "and \"where\" is another BoolExp expression. "
          <> "All other properties represent fields where the property name represents a column name, and the value represents a row value."

-- | Permissions get translated into boolean expressions that are threaded throuhgout the
-- parsers. For the leaf values of those permissions, we use this type, which references but doesn't
-- inline the session variables.
data PartialSQLExp (backend :: BackendType)
  = PSESessVar (SessionVarType backend) SessionVariable
  | PSESession
  | PSESQLExp (SQLExpression backend)
  deriving (Generic)

deriving instance (Backend b) => Eq (PartialSQLExp b)

deriving instance (Backend b) => Show (PartialSQLExp b)

instance
  ( Backend b
  ) =>
  NFData (PartialSQLExp b)

instance
  ( Backend b
  ) =>
  Hashable (PartialSQLExp b)

instance (Backend b) => ToJSON (PartialSQLExp b) where
  toJSON = \case
    PSESessVar colTy sessVar -> toJSON (colTy, sessVar)
    PSESession -> String "hasura_session"
    PSESQLExp e -> toJSON e

isStaticValue :: PartialSQLExp backend -> Bool
isStaticValue = \case
  PSESessVar _ _ -> False
  PSESession -> False
  PSESQLExp _ -> True

hasStaticExp :: (Backend b) => OpExpG b (PartialSQLExp b) -> Bool
hasStaticExp = getAny . foldMap (Any . isStaticValue)

----------------------------------------------------------------------------------------------------
-- Boolean expressions in the schema

-- | Operand for cast operator
type CastExp backend field = HashMap.HashMap (ScalarType backend) [OpExpG backend field]

data ComparisonNullability = NonNullableComparison | NullableComparison
  deriving (Generic)

deriving instance Show ComparisonNullability

deriving instance Eq ComparisonNullability

instance NFData ComparisonNullability

instance Hashable ComparisonNullability

instance ToJSON ComparisonNullability

-- | This type represents the boolean operators that can be applied on values of a column. This type
-- only contains the common core, that we expect to be ultimately entirely supported in most if not
-- all backends. Backends can extend this with the @BooleanOperators@ type in @Backend@.
data OpExpG (backend :: BackendType) field
  = ACast (CastExp backend field)
  | AEQ ComparisonNullability field
  | ANE ComparisonNullability field
  | AIN field
  | ANIN field
  | AGT field
  | ALT field
  | AGTE field
  | ALTE field
  | ALIKE field -- LIKE
  | ANLIKE field -- NOT LIKE
  | CEQ (RootOrCurrentColumn backend)
  | CNE (RootOrCurrentColumn backend)
  | CGT (RootOrCurrentColumn backend)
  | CLT (RootOrCurrentColumn backend)
  | CGTE (RootOrCurrentColumn backend)
  | CLTE (RootOrCurrentColumn backend)
  | ANISNULL -- IS NULL
  | ANISNOTNULL -- IS NOT NULL
  | ABackendSpecific (BooleanOperators backend field)
  deriving (Generic)

-- NOTE: There is no redaction expression ('AnnRedactionExp') required for the
-- column involved here, because RootOrCurrentColumn is only used in the permissions
-- system, where no redaction is applied anyway.
-- If we start using this type in normal GraphQL 'where' bool exps, we will need
-- to add a redaction expression here to deal with redaction from inherited roles.
data RootOrCurrentColumn b = RootOrCurrentColumn RootOrCurrent (Column b)
  deriving (Generic)

deriving instance (Backend b) => Show (RootOrCurrentColumn b)

deriving instance (Backend b) => Eq (RootOrCurrentColumn b)

instance (Backend b) => NFData (RootOrCurrentColumn b)

instance (Backend b) => Hashable (RootOrCurrentColumn b)

instance (Backend b) => ToJSON (RootOrCurrentColumn b)

-- | The arguments of column-operators may refer to either the so-called 'root
-- tabular value' or 'current tabular value'.
data RootOrCurrent = IsRoot | IsCurrent
  deriving (Eq, Show, Generic)

instance NFData RootOrCurrent

instance Hashable RootOrCurrent

instance ToJSON RootOrCurrent

deriving instance (Backend b) => Functor (OpExpG b)

deriving instance (Backend b) => Foldable (OpExpG b)

deriving instance (Backend b) => Traversable (OpExpG b)

deriving instance
  ( Backend b,
    Show (BooleanOperators b a),
    Show a
  ) =>
  Show (OpExpG b a)

deriving instance
  ( Backend b,
    Eq (BooleanOperators b a),
    Eq a
  ) =>
  Eq (OpExpG b a)

instance
  ( Backend b,
    NFData (BooleanOperators b a),
    NFData a
  ) =>
  NFData (OpExpG b a)

instance
  ( Backend b,
    Hashable (BooleanOperators b a),
    Hashable a
  ) =>
  Hashable (OpExpG b a)

instance
  ( Backend b,
    ToJSONKeyValue (BooleanOperators b a),
    ToJSON a
  ) =>
  ToJSONKeyValue (OpExpG b a)
  where
  toJSONKeyValue = \case
    ACast a -> ("_cast", toJSON $ object . map toJSONKeyValue <$> a)
    AEQ _ a -> ("_eq", toJSON a)
    ANE _ a -> ("_ne", toJSON a)
    AIN a -> ("_in", toJSON a)
    ANIN a -> ("_nin", toJSON a)
    AGT a -> ("_gt", toJSON a)
    ALT a -> ("_lt", toJSON a)
    AGTE a -> ("_gte", toJSON a)
    ALTE a -> ("_lte", toJSON a)
    ALIKE a -> ("_like", toJSON a)
    ANLIKE a -> ("_nlike", toJSON a)
    CEQ a -> ("_ceq", toJSON a)
    CNE a -> ("_cne", toJSON a)
    CGT a -> ("_cgt", toJSON a)
    CLT a -> ("_clt", toJSON a)
    CGTE a -> ("_cgte", toJSON a)
    CLTE a -> ("_clte", toJSON a)
    ANISNULL -> ("_is_null", toJSON True)
    ANISNOTNULL -> ("_is_null", toJSON False)
    ABackendSpecific b -> toJSONKeyValue b

opExpDepCol :: OpExpG backend field -> Maybe (RootOrCurrentColumn backend)
opExpDepCol = \case
  CEQ c -> Just c
  CNE c -> Just c
  CGT c -> Just c
  CLT c -> Just c
  CGTE c -> Just c
  CLTE c -> Just c
  _ -> Nothing

-- | This type is used to represent the kinds of boolean expression used for computed fields
-- based on the return type of the SQL function.
data ComputedFieldBoolExp (backend :: BackendType) scalar
  = -- | SQL function returning a scalar
    CFBEScalar (AnnRedactionExp backend scalar) [OpExpG backend scalar]
  | -- | SQL function returning SET OF table
    CFBETable (TableName backend) (AnnBoolExp backend scalar)
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance
  ( Backend b,
    Eq (AnnBoolExp b a),
    Eq (OpExpG b a)
  ) =>
  Eq (ComputedFieldBoolExp b a)

deriving instance
  ( Backend b,
    Show (AnnBoolExp b a),
    Show (OpExpG b a)
  ) =>
  Show (ComputedFieldBoolExp b a)

instance
  ( Backend b,
    NFData (AnnBoolExp b a),
    NFData (OpExpG b a)
  ) =>
  NFData (ComputedFieldBoolExp b a)

instance
  ( Backend b,
    Hashable (AnnBoolExp b a),
    Hashable (OpExpG b a)
  ) =>
  Hashable (ComputedFieldBoolExp b a)

-- | Using a computed field in boolean expression.
-- Example: A computed field "full_name" ("first_name" || "last_name") is defined to the "user"
-- table. Boolean expression to filter whose "full_name" is LIKE "%bob%"
-- query {
--   user(where: {full_name: {_like: "%bob%"}}){
--       id
--       first_name
--       last_name
--       full_name
--   }
-- }
-- Limitation: We only support computed fields in boolean expressions when they
-- are functions with no input arguments, because it is complex to generate schema
-- for @where@ clauses for functions that have input arguments.
data AnnComputedFieldBoolExp (backend :: BackendType) scalar = AnnComputedFieldBoolExp
  { _acfbXFieldInfo :: XComputedField backend,
    _acfbName :: ComputedFieldName,
    _acfbFunction :: FunctionName backend,
    _acfbFunctionArgsExp :: FunctionArgsExp backend scalar,
    _acfbBoolExp :: ComputedFieldBoolExp backend scalar
  }
  deriving (Generic)

deriving instance (Backend b) => Functor (AnnComputedFieldBoolExp b)

deriving instance (Backend b) => Foldable (AnnComputedFieldBoolExp b)

deriving instance (Backend b) => Traversable (AnnComputedFieldBoolExp b)

deriving instance
  ( Backend b,
    Eq (ComputedFieldBoolExp b a),
    Eq (FunctionArgsExp b a)
  ) =>
  Eq (AnnComputedFieldBoolExp b a)

deriving instance
  ( Backend b,
    Show (ComputedFieldBoolExp b a),
    Show (FunctionArgsExp b a)
  ) =>
  Show (AnnComputedFieldBoolExp b a)

instance
  ( Backend b,
    NFData (ComputedFieldBoolExp b a),
    NFData (FunctionArgsExp b a)
  ) =>
  NFData (AnnComputedFieldBoolExp b a)

instance
  ( Backend b,
    Hashable (ComputedFieldBoolExp b a),
    Hashable (FunctionArgsExp b a)
  ) =>
  Hashable (AnnComputedFieldBoolExp b a)

data RemoteRelPermBoolExp b field = RemoteRelPermBoolExp
  { rawBoolExp :: (RelName, Value),
    lhsCol :: (Column b, ColumnType b),
    rhsFetchInfo :: AB.AnyBackend (RemoteRelRHSFetchInfo field)
  }
  deriving (Generic)

deriving instance (Backend b) => Functor (RemoteRelPermBoolExp b)

deriving instance (Backend b) => Foldable (RemoteRelPermBoolExp b)

deriving instance (Backend b) => Traversable (RemoteRelPermBoolExp b)

instance Eq (RemoteRelPermBoolExp b f) where
  (RemoteRelPermBoolExp rawBoolExp1 _ _) == (RemoteRelPermBoolExp rawBoolExp2 _ _) = rawBoolExp1 == rawBoolExp2

instance Hashable (RemoteRelPermBoolExp b a) where
  hashWithSalt salt (RemoteRelPermBoolExp rawBoolExp _ _) = hashWithSalt salt rawBoolExp

instance NFData (RemoteRelPermBoolExp b a) where
  rnf (RemoteRelPermBoolExp rawBoolExp _ _) = rnf rawBoolExp

instance (Show (RemoteRelPermBoolExp b f)) where
  show (RemoteRelPermBoolExp boolExp _ _) = show (boolExp)

data RemoteRelRHSFetchInfo field b = RemoteRelRHSFetchInfo
  { rrrfiColumn :: (ScalarType b, Column b),
    rrrfiTable :: TableName b,
    rrrfiWhere :: RemoteRelRHSFetchWhereExp (Column b),
    rrrfiSource :: SourceName,
    rrrfiSourceConfig :: SourceConfig b
  }

-- | This type is used for boolean terms in GBoolExp in the schema; there are four kinds boolean
-- terms:
--   - operators on a column of the current table, using the 'OpExpG' kind of operators
--   - arbitrary expressions on columns of tables in relationships (in the same source)
--   - A computed field of the current table
--   - aggregation operations on array relationships on the current tables.
--
-- This type is parameterized over the type of leaf values, the values on which we operate.
data AnnBoolExpFld (backend :: BackendType) leaf
  = AVColumn (ColumnInfo backend) (AnnRedactionExp backend leaf) [OpExpG backend leaf]
  | AVNestedObject (NestedObjectInfo backend) (AnnBoolExp backend leaf)
  | AVRelationship
      (RelInfo backend)
      (RelationshipFilters backend leaf)
  | AVComputedField (AnnComputedFieldBoolExp backend leaf)
  | AVAggregationPredicates (AggregationPredicates backend leaf)
  | AVRemoteRelationship (RemoteRelPermBoolExp backend leaf)
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance
  ( Backend b,
    Eq (AggregationPredicates b a),
    Eq (AnnBoolExp b a),
    Eq (AnnComputedFieldBoolExp b a),
    Eq (OpExpG b a),
    Eq (RemoteRelPermBoolExp b a)
  ) =>
  Eq (AnnBoolExpFld b a)

deriving instance
  ( Backend b,
    Show (AggregationPredicates b a),
    Show (AnnBoolExp b a),
    Show (AnnComputedFieldBoolExp b a),
    Show (OpExpG b a),
    Show (RemoteRelPermBoolExp b a)
  ) =>
  Show (AnnBoolExpFld b a)

instance
  ( Backend b,
    NFData (AggregationPredicates b a),
    NFData (AnnBoolExp b a),
    NFData (AnnComputedFieldBoolExp b a),
    NFData (OpExpG b a),
    NFData (RemoteRelPermBoolExp b a)
  ) =>
  NFData (AnnBoolExpFld b a)

instance
  ( Backend b,
    Hashable (AggregationPredicates b a),
    Hashable (AnnBoolExp b a),
    Hashable (AnnComputedFieldBoolExp b a),
    Hashable (OpExpG b a),
    Hashable (RemoteRelPermBoolExp b a)
  ) =>
  Hashable (AnnBoolExpFld b a)

instance
  ( Backend b,
    ToJSONKeyValue (AggregationPredicates b a),
    ToJSONKeyValue (OpExpG b a),
    ToJSON a
  ) =>
  ToJSONKeyValue (AnnBoolExpFld b a)
  where
  toJSONKeyValue = \case
    AVColumn pci _redactionExp opExps ->
      ( K.fromText $ toTxt $ ciColumn pci,
        toJSON (pci, object . pure . toJSONKeyValue <$> opExps)
      )
    AVNestedObject noi boolExp ->
      ( K.fromText $ toTxt $ _noiColumn noi,
        toJSON (noi, boolExp)
      )
    AVRelationship ri filters ->
      ( K.fromText $ relNameToTxt $ riName ri,
        toJSON (ri, toJSON filters)
      )
    AVComputedField cfBoolExp ->
      ( K.fromText $ toTxt $ _acfbName cfBoolExp,
        let function = _acfbFunction cfBoolExp
         in case _acfbBoolExp cfBoolExp of
              CFBEScalar _redactionExp opExps -> toJSON (function, object . pure . toJSONKeyValue <$> opExps)
              CFBETable _ boolExp -> toJSON (function, toJSON boolExp)
      )
    AVAggregationPredicates avAggregationPredicates -> toJSONKeyValue avAggregationPredicates
    AVRemoteRelationship (RemoteRelPermBoolExp (relName, fieldValue) _ _) ->
      (K.fromText (relNameToTxt relName), fieldValue)

-- | This type represents a boolean expression over a relationship. In addition
-- to the actual user-specified predicate, we need to also consider the
-- permissions of the target table.
--
-- Because the permissions may include column-comparison-operators, they need to
-- be translated in the context of the table they apply to. Thus we keep the
-- permissions and filters separate.
data RelationshipFilters (backend :: BackendType) leaf = RelationshipFilters
  { rfTargetTablePermissions :: AnnBoolExp backend leaf,
    rfFilter :: AnnBoolExp backend leaf
  }
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance
  ( Backend b,
    Eq (AnnBoolExp b a)
  ) =>
  Eq (RelationshipFilters b a)

deriving instance
  ( Backend b,
    Show (AnnBoolExp b a)
  ) =>
  Show (RelationshipFilters b a)

instance
  ( Backend b,
    NFData (AnnBoolExp b a)
  ) =>
  NFData (RelationshipFilters b a)

instance
  ( Backend b,
    Hashable (AnnBoolExp b a)
  ) =>
  Hashable (RelationshipFilters b a)

instance (ToJSON (AnnBoolExp backend leaf)) => ToJSON (RelationshipFilters backend leaf)

-- | A simple alias for the kind of boolean expressions used in the schema, that ties together
-- 'GBoolExp', 'OpExpG', and 'AnnBoolExpFld'.
type AnnBoolExp backend scalar = GBoolExp backend (AnnBoolExpFld backend scalar)

-- Type aliases for common use cases:
type AnnBoolExpFldSQL backend = AnnBoolExpFld backend (SQLExpression backend)

type AnnBoolExpSQL backend = AnnBoolExp backend (SQLExpression backend)

type AnnBoolExpPartialSQL backend = AnnBoolExp backend (PartialSQLExp backend)

annBoolExpTrue :: AnnBoolExp backend scalar
annBoolExpTrue = gBoolExpTrue

andAnnBoolExps :: AnnBoolExp backend scalar -> AnnBoolExp backend scalar -> AnnBoolExp backend scalar
andAnnBoolExps l r = BoolAnd [l, r]

----------------------------------------------------------------------------------------------------
-- Operands for specific operators

-- Arguably, most of those should be moved elsewhere, since not all of the corresponding operators
-- are part of the common core of operators.

-- | Operand for STDWithin opoerator
data DWithinGeomOp field = DWithinGeomOp
  { dwgeomDistance :: field,
    dwgeomFrom :: field
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)

instance (NFData a) => NFData (DWithinGeomOp a)

instance (Hashable a) => Hashable (DWithinGeomOp a)

instance (FromJSON a) => FromJSON (DWithinGeomOp a) where
  parseJSON = genericParseJSON hasuraJSON

instance (ToJSON a) => ToJSON (DWithinGeomOp a) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

-- | Operand for STDWithin opoerator
data DWithinGeogOp field = DWithinGeogOp
  { dwgeogDistance :: field,
    dwgeogFrom :: field,
    dwgeogUseSpheroid :: field
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)

instance (NFData a) => NFData (DWithinGeogOp a)

instance (Hashable a) => Hashable (DWithinGeogOp a)

instance (FromJSON a) => FromJSON (DWithinGeogOp a) where
  parseJSON = genericParseJSON hasuraJSON

instance (ToJSON a) => ToJSON (DWithinGeogOp a) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

-- | Operand for STIntersect
data STIntersectsNbandGeommin field = STIntersectsNbandGeommin
  { singNband :: field,
    singGeommin :: field
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)

instance (NFData a) => NFData (STIntersectsNbandGeommin a)

instance (Hashable a) => Hashable (STIntersectsNbandGeommin a)

instance (FromJSON field) => FromJSON (STIntersectsNbandGeommin field) where
  parseJSON = genericParseJSON hasuraJSON

instance (ToJSON field) => ToJSON (STIntersectsNbandGeommin field) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

-- | Operand for STIntersect
data STIntersectsGeomminNband field = STIntersectsGeomminNband
  { signGeommin :: field,
    signNband :: Maybe field
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)

instance (NFData a) => NFData (STIntersectsGeomminNband a)

instance (Hashable a) => Hashable (STIntersectsGeomminNband a)

instance (FromJSON field) => FromJSON (STIntersectsGeomminNband field) where
  parseJSON = genericParseJSON hasuraJSON

instance (ToJSON field) => ToJSON (STIntersectsGeomminNband field) where
  toJSON = genericToJSON hasuraJSON
  toEncoding = genericToEncoding hasuraJSON

----------------------------------------------------------------------------------------------------
-- Miscellaneous

-- | This captures a boolean expression where, if it is false, some associated data needs to be redacted
-- (in practice, nulled out) because the user doesn't have access to it. Alternatively,
-- "no redaction" is explicitly defined, which is used as an optimization to avoid evaluating a boolexp
-- if unnecessary (as opposed to defining a boolean exp which always evaluates to true).

-- See notes [Inherited roles architecture for read queries] and [SQL generation for inherited roles]
-- for more information about what this is used for.
data AnnRedactionExp b v
  = NoRedaction
  | RedactIfFalse (GBoolExp b (AnnBoolExpFld b v))
  deriving stock (Functor, Foldable, Traversable, Generic)

deriving stock instance (Backend b, Show (GBoolExp b (AnnBoolExpFld b v))) => Show (AnnRedactionExp b v)

deriving stock instance (Backend b, Eq (GBoolExp b (AnnBoolExpFld b v))) => Eq (AnnRedactionExp b v)

instance (Backend b, Hashable (GBoolExp b (AnnBoolExpFld b v))) => Hashable (AnnRedactionExp b v)

instance (Backend b, NFData (GBoolExp b (AnnBoolExpFld b v))) => NFData (AnnRedactionExp b v)

instance (Backend b, ToJSON (GBoolExp b (AnnBoolExpFld b v))) => ToJSON (AnnRedactionExp b v) where
  toJSON = \case
    NoRedaction -> Null
    RedactIfFalse boolExp -> toJSON boolExp

-- misc type aliases
type AnnRedactionExpPartialSQL b = AnnRedactionExp b (PartialSQLExp b)

type AnnRedactionExpUnpreparedValue b = AnnRedactionExp b (UnpreparedValue b)

type PreSetColsG b v = HashMap.HashMap (Column b) v

type PreSetColsPartial b = HashMap.HashMap (Column b) (PartialSQLExp b)
