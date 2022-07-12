{-# LANGUAGE TemplateHaskell #-}
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
    geWhere,
    geTable,
    _BoolExists,
    DWithinGeomOp (..),
    DWithinGeogOp (..),
    CastExp,
    OpExpG (..),
    opExpDepCol,
    STIntersectsNbandGeommin (..),
    STIntersectsGeomminNband (..),
    ComputedFieldBoolExp (..),
    AnnComputedFieldBoolExp (..),
    AnnBoolExpFld (..),
    AnnBoolExp,
    AnnColumnCaseBoolExpPartialSQL,
    AnnColumnCaseBoolExp,
    AnnColumnCaseBoolExpField (..),
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
  )
where

import Control.Lens.Plated
import Control.Lens.TH
import Data.Aeson.Extended
import Data.Aeson.Internal
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.TH
import Data.HashMap.Strict qualified as M
import Data.Monoid
import Data.Text.Extended
import Hasura.Incremental (Cacheable)
import Hasura.Prelude
import Hasura.RQL.Types.Backend
import Hasura.RQL.Types.Column
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.ComputedField
import Hasura.RQL.Types.Function
import Hasura.RQL.Types.Relationships.Local
import Hasura.SQL.Backend
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

instance (Backend b, Cacheable a) => Cacheable (GBoolExp b a)

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
      let m = M.fromList $ map getKV bExps
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

instance (Backend b, Cacheable a) => Cacheable (GExists b a)

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

makeLenses ''GExists

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

instance Cacheable ColExp

instance FromJSONKeyValue ColExp where
  parseJSONKeyValue (k, v) = ColExp (FieldName (K.toText k)) <$> parseJSON v

instance ToJSONKeyValue ColExp where
  toJSONKeyValue (ColExp k v) = (K.fromText (getFieldNameTxt k), v)

-- | This @BoolExp@ type is a simple alias for the boolean expressions used in permissions, that
-- uses 'ColExp' as the term in GBoolExp.
newtype BoolExp (b :: BackendType) = BoolExp {unBoolExp :: GBoolExp b ColExp}
  deriving newtype (Show, Eq, Generic, NFData, Cacheable, ToJSON, FromJSON)

$(makeWrapped ''BoolExp)

makePrisms ''GBoolExp

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

instance (Backend b, NFData (BooleanOperators b (PartialSQLExp b))) => NFData (PartialSQLExp b)

instance (Backend b, Hashable (BooleanOperators b (PartialSQLExp b))) => Hashable (PartialSQLExp b)

instance (Backend b, Cacheable (BooleanOperators b (PartialSQLExp b))) => Cacheable (PartialSQLExp b)

instance Backend b => ToJSON (PartialSQLExp b) where
  toJSON = \case
    PSESessVar colTy sessVar -> toJSON (colTy, sessVar)
    PSESession -> String "hasura_session"
    PSESQLExp e -> toJSON e

isStaticValue :: PartialSQLExp backend -> Bool
isStaticValue = \case
  PSESessVar _ _ -> False
  PSESession -> False
  PSESQLExp _ -> True

hasStaticExp :: Backend b => OpExpG b (PartialSQLExp b) -> Bool
hasStaticExp = getAny . foldMap (Any . isStaticValue)

----------------------------------------------------------------------------------------------------
-- Boolean expressions in the schema

-- | Operand for cast operator
type CastExp backend field = M.HashMap (ScalarType backend) [OpExpG backend field]

-- | This type represents the boolean operators that can be applied on values of a column. This type
-- only contains the common core, that we expect to be ultimately entirely supported in most if not
-- all backends. Backends can extend this with the @BooleanOperators@ type in @Backend@.
data OpExpG (backend :: BackendType) field
  = ACast (CastExp backend field)
  | AEQ Bool field
  | ANE Bool field
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

data RootOrCurrentColumn b = RootOrCurrentColumn RootOrCurrent (Column b)
  deriving (Generic)

deriving instance Backend b => Show (RootOrCurrentColumn b)

deriving instance Backend b => Eq (RootOrCurrentColumn b)

instance Backend b => NFData (RootOrCurrentColumn b)

instance Backend b => Cacheable (RootOrCurrentColumn b)

instance Backend b => Hashable (RootOrCurrentColumn b)

instance Backend b => ToJSON (RootOrCurrentColumn b)

-- | The arguments of column-operators may refer to either the so-called 'root
-- tabular value' or 'current tabular value'.
data RootOrCurrent = IsRoot | IsCurrent
  deriving (Eq, Show, Generic)

instance NFData RootOrCurrent

instance Cacheable RootOrCurrent

instance Hashable RootOrCurrent

instance ToJSON RootOrCurrent

deriving instance (Backend b) => Functor (OpExpG b)

deriving instance (Backend b) => Foldable (OpExpG b)

deriving instance (Backend b) => Traversable (OpExpG b)

deriving instance (Backend b, Show (BooleanOperators b a), Show a) => Show (OpExpG b a)

deriving instance (Backend b, Eq (BooleanOperators b a), Eq a) => Eq (OpExpG b a)

instance (Backend b, NFData (BooleanOperators b a), NFData a) => NFData (OpExpG b a)

instance (Backend b, Cacheable (BooleanOperators b a), Cacheable a) => Cacheable (OpExpG b a)

instance (Backend b, Hashable (BooleanOperators b a), Hashable a) => Hashable (OpExpG b a)

instance (Backend b, ToJSONKeyValue (BooleanOperators b a), ToJSON a) => ToJSONKeyValue (OpExpG b a) where
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

-- | This type is used to represent the kinds of boolean expression used for compouted fields
-- based on the return type of the SQL function.
data ComputedFieldBoolExp (backend :: BackendType) scalar
  = -- | SQL function returning a scalar
    CFBEScalar [OpExpG backend scalar]
  | -- | SQL function returning SET OF table
    CFBETable (TableName backend) (AnnBoolExp backend scalar)
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance (Backend b, Eq (BooleanOperators b a), Eq (FunctionArgumentExp b a), Eq a) => Eq (ComputedFieldBoolExp b a)

deriving instance (Backend b, Show (BooleanOperators b a), Show (FunctionArgumentExp b a), Show a) => Show (ComputedFieldBoolExp b a)

instance (Backend b, NFData (BooleanOperators b a), NFData (FunctionArgumentExp b a), NFData a) => NFData (ComputedFieldBoolExp b a)

instance (Backend b, Cacheable (BooleanOperators b a), Cacheable (FunctionArgumentExp b a), Cacheable a) => Cacheable (ComputedFieldBoolExp b a)

instance (Backend b, Hashable (BooleanOperators b a), Hashable (FunctionArgumentExp b a), Hashable a) => Hashable (ComputedFieldBoolExp b a)

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

deriving instance (Backend b, Eq (BooleanOperators b a), Eq (FunctionArgumentExp b a), Eq a) => Eq (AnnComputedFieldBoolExp b a)

deriving instance (Backend b, Show (BooleanOperators b a), Show (FunctionArgumentExp b a), Show a) => Show (AnnComputedFieldBoolExp b a)

instance (Backend b, NFData (BooleanOperators b a), NFData (FunctionArgumentExp b a), NFData a) => NFData (AnnComputedFieldBoolExp b a)

instance (Backend b, Cacheable (BooleanOperators b a), Cacheable (FunctionArgumentExp b a), Cacheable a) => Cacheable (AnnComputedFieldBoolExp b a)

instance (Backend b, Hashable (BooleanOperators b a), Hashable (FunctionArgumentExp b a), Hashable a) => Hashable (AnnComputedFieldBoolExp b a)

-- | This type is used for boolean terms in GBoolExp in the schema; there are two kinds boolean
-- terms:
--   - operators on a column of the current table, using the 'OpExpG' kind of operators
--   - arbitrary expressions on columns of tables in relationships (in the same source)
--   - A computed field of the current table
--
-- This type is parameterized over the type of leaf values, the values on which we operate.
data AnnBoolExpFld (backend :: BackendType) leaf
  = AVColumn (ColumnInfo backend) [OpExpG backend leaf]
  | AVRelationship (RelInfo backend) (AnnBoolExp backend leaf)
  | AVComputedField (AnnComputedFieldBoolExp backend leaf)
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance (Backend b, Eq (BooleanOperators b a), Eq (FunctionArgumentExp b a), Eq a) => Eq (AnnBoolExpFld b a)

deriving instance (Backend b, Show (BooleanOperators b a), Show (FunctionArgumentExp b a), Show a) => Show (AnnBoolExpFld b a)

instance (Backend b, NFData (BooleanOperators b a), NFData (FunctionArgumentExp b a), NFData a) => NFData (AnnBoolExpFld b a)

instance (Backend b, Cacheable (BooleanOperators b a), Cacheable (FunctionArgumentExp b a), Cacheable a) => Cacheable (AnnBoolExpFld b a)

instance (Backend b, Hashable (BooleanOperators b a), Hashable (FunctionArgumentExp b a), Hashable a) => Hashable (AnnBoolExpFld b a)

instance (Backend b, ToJSONKeyValue (BooleanOperators b a), ToJSON a) => ToJSONKeyValue (AnnBoolExpFld b a) where
  toJSONKeyValue = \case
    AVColumn pci opExps ->
      ( K.fromText $ toTxt $ ciColumn pci,
        toJSON (pci, object . pure . toJSONKeyValue <$> opExps)
      )
    AVRelationship ri relBoolExp ->
      ( K.fromText $ relNameToTxt $ riName ri,
        toJSON (ri, toJSON relBoolExp)
      )
    AVComputedField cfBoolExp ->
      ( K.fromText $ toTxt $ _acfbName cfBoolExp,
        let function = _acfbFunction cfBoolExp
         in case _acfbBoolExp cfBoolExp of
              CFBEScalar opExps -> toJSON (function, object . pure . toJSONKeyValue <$> opExps)
              CFBETable _ boolExp -> toJSON (function, toJSON boolExp)
      )

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

instance (Cacheable a) => Cacheable (DWithinGeomOp a)

instance (Hashable a) => Hashable (DWithinGeomOp a)

$(deriveJSON hasuraJSON ''DWithinGeomOp)

-- | Operand for STDWithin opoerator
data DWithinGeogOp field = DWithinGeogOp
  { dwgeogDistance :: field,
    dwgeogFrom :: field,
    dwgeogUseSpheroid :: field
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)

instance (NFData a) => NFData (DWithinGeogOp a)

instance (Cacheable a) => Cacheable (DWithinGeogOp a)

instance (Hashable a) => Hashable (DWithinGeogOp a)

$(deriveJSON hasuraJSON ''DWithinGeogOp)

-- | Operand for STIntersect
data STIntersectsNbandGeommin field = STIntersectsNbandGeommin
  { singNband :: field,
    singGeommin :: field
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)

instance (NFData a) => NFData (STIntersectsNbandGeommin a)

instance (Cacheable a) => Cacheable (STIntersectsNbandGeommin a)

instance (Hashable a) => Hashable (STIntersectsNbandGeommin a)

$(deriveJSON hasuraJSON ''STIntersectsNbandGeommin)

-- | Operand for STIntersect
data STIntersectsGeomminNband field = STIntersectsGeomminNband
  { signGeommin :: field,
    signNband :: Maybe field
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)

instance (NFData a) => NFData (STIntersectsGeomminNband a)

instance (Cacheable a) => Cacheable (STIntersectsGeomminNband a)

instance (Hashable a) => Hashable (STIntersectsGeomminNband a)

$(deriveJSON hasuraJSON ''STIntersectsGeomminNband)

----------------------------------------------------------------------------------------------------
-- Miscellaneous

-- | This is a simple newtype over AnnBoolExpFld. At time of writing, I do not know why we want
-- this, and why it exists. It might be a relic of a needed differentiation, now lost?
-- TODO: can this be removed?
newtype AnnColumnCaseBoolExpField (backend :: BackendType) field = AnnColumnCaseBoolExpField {_accColCaseBoolExpField :: AnnBoolExpFld backend field}
  deriving (Functor, Foldable, Traversable, Generic)

deriving instance (Backend b, Eq (BooleanOperators b a), Eq (FunctionArgumentExp b a), Eq a) => Eq (AnnColumnCaseBoolExpField b a)

deriving instance (Backend b, Show (BooleanOperators b a), Show (FunctionArgumentExp b a), Show a) => Show (AnnColumnCaseBoolExpField b a)

instance (Backend b, NFData (BooleanOperators b a), NFData (FunctionArgumentExp b a), NFData a) => NFData (AnnColumnCaseBoolExpField b a)

instance (Backend b, Cacheable (BooleanOperators b a), Cacheable (FunctionArgumentExp b a), Cacheable a) => Cacheable (AnnColumnCaseBoolExpField b a)

instance (Backend b, Hashable (BooleanOperators b a), Hashable (FunctionArgumentExp b a), Hashable a) => Hashable (AnnColumnCaseBoolExpField b a)

instance (Backend b, ToJSONKeyValue (BooleanOperators b a), ToJSON a) => ToJSONKeyValue (AnnColumnCaseBoolExpField b a) where
  toJSONKeyValue = toJSONKeyValue . _accColCaseBoolExpField

-- | Similar to AnnBoolExp, this type alias ties together
-- 'GBoolExp', 'OpExpG', and 'AnnColumnCaseBoolExpFld'.
type AnnColumnCaseBoolExp b a = GBoolExp b (AnnColumnCaseBoolExpField b a)

-- misc type aliases
type AnnColumnCaseBoolExpPartialSQL b = AnnColumnCaseBoolExp b (PartialSQLExp b)

type PreSetColsG b v = M.HashMap (Column b) v

type PreSetColsPartial b = M.HashMap (Column b) (PartialSQLExp b)
