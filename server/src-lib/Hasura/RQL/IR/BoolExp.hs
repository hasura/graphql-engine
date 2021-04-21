{-# LANGUAGE UndecidableInstances #-}

module Hasura.RQL.IR.BoolExp
  ( BoolExp(..)
  , ColExp(..)
  , GBoolExp(..)
  , gBoolExpTrue
  , GExists(..)

  , geWhere
  , geTable
  , _BoolExists

  , DWithinGeomOp(..)
  , DWithinGeogOp(..)
  , CastExp
  , OpExpG(..)
  , opExpDepCol
  , STIntersectsNbandGeommin(..)
  , STIntersectsGeomminNband(..)

  , AnnBoolExpFld(..)
  , AnnBoolExp
  , AnnColumnCaseBoolExpPartialSQL
  , AnnColumnCaseBoolExp
  , AnnColumnCaseBoolExpField(..)
  , traverseAnnBoolExp
  , fmapAnnBoolExp
  , traverseAnnColumnCaseBoolExp
  , fmapAnnColumnCaseBoolExp
  , annBoolExpTrue
  , andAnnBoolExps

  , AnnBoolExpFldSQL
  , AnnBoolExpSQL
  , PartialSQLExp(..)
  , isStaticValue
  , hasStaticExp
  , AnnBoolExpPartialSQL

  , PreSetColsG
  , PreSetColsPartial
  ) where

import           Hasura.Prelude

import qualified Data.HashMap.Strict           as M

import           Control.Lens.Plated
import           Control.Lens.TH
import           Data.Aeson.Extended
import           Data.Aeson.Internal
import           Data.Aeson.TH
import           Data.Monoid
import           Data.Text.Extended
import           Data.Typeable

import           Hasura.Incremental            (Cacheable)
import           Hasura.RQL.Types.Backend
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Relationship
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Session



----------------------------------------------------------------------------------------------------
-- Boolean structure

-- | This type represents a hierarchical boolean expression. It is parametric over the actual
-- implementation of the actual boolean term values. It nonetheless leaks some information:
-- "exists" is only used in permissions, to add conditions based on another table.
data GBoolExp (b :: BackendType) a
  = BoolAnd    ![GBoolExp b a]
  | BoolOr     ![GBoolExp b a]
  | BoolNot    !(GBoolExp b a)
  | BoolExists !(GExists  b a)
  | BoolFld    !a
  deriving (Show, Eq, Functor, Foldable, Traversable, Data, Generic)
instance (Backend b, NFData    a) => NFData    (GBoolExp b a)
instance (Backend b, Data      a) => Plated    (GBoolExp b a)
instance (Backend b, Cacheable a) => Cacheable (GBoolExp b a)
instance (Backend b, Hashable  a) => Hashable  (GBoolExp b a)

instance (Backend b, FromJSONKeyValue a) => FromJSON (GBoolExp b a) where
  parseJSON = withObject "boolean expression" \o ->
    BoolAnd <$> forM (M.toList o) \(k, v) ->
      if | k == "$or"     -> BoolOr     <$> parseJSON v <?> Key k
         | k == "_or"     -> BoolOr     <$> parseJSON v <?> Key k
         | k == "$and"    -> BoolAnd    <$> parseJSON v <?> Key k
         | k == "_and"    -> BoolAnd    <$> parseJSON v <?> Key k
         | k == "$not"    -> BoolNot    <$> parseJSON v <?> Key k
         | k == "_not"    -> BoolNot    <$> parseJSON v <?> Key k
         | k == "$exists" -> BoolExists <$> parseJSON v <?> Key k
         | k == "_exists" -> BoolExists <$> parseJSON v <?> Key k
         | otherwise      -> BoolFld    <$> parseJSONKeyValue (k, v)

instance (Backend b, ToJSONKeyValue a) => ToJSON (GBoolExp b a) where
  toJSON be = case be of
    -- special encoding for _and
    BoolAnd bExps ->
      let m = M.fromList $ map getKV bExps
      -- if the keys aren't repeated, then object encoding can be used
      in if length m == length bExps
         then toJSON m
         else object $ pure kv
    _ -> object $ pure kv
    where
      kv = getKV be
      getKV = \case
        BoolAnd    bExps   -> "_and"    .= map toJSON bExps
        BoolOr     bExps   -> "_or"     .= map toJSON bExps
        BoolNot    bExp    -> "_not"    .= toJSON bExp
        BoolExists bExists -> "_exists" .= toJSON bExists
        BoolFld    a       -> toJSONKeyValue a

gBoolExpTrue :: GBoolExp b a
gBoolExpTrue = BoolAnd []


-- | Represents a condition on an aribtrary table. Used as part of our permissions boolean
-- expressions. See our documentation for more information:
-- https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#using-unrelated-tables-views
data GExists (b :: BackendType) a
  = GExists
  { _geTable :: !(TableName b)
  , _geWhere :: !(GBoolExp b a)
  } deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Show a) => Show (GExists b a)
deriving instance (Backend b, Eq   a) => Eq   (GExists b a)
deriving instance (Backend b, Typeable a, Data a) => Data (GExists b a)
instance (Backend b, NFData    a) => NFData    (GExists b a)
instance (Backend b, Data      a) => Plated    (GExists b a)
instance (Backend b, Cacheable a) => Cacheable (GExists b a)
instance (Backend b, Hashable  a) => Hashable  (GExists b a)

instance (Backend b, FromJSONKeyValue a) => FromJSON (GExists b a) where
  parseJSON = withObject "_exists" \o -> do
    qt <- o .: "_table"
    wh <- o .: "_where"
    pure $ GExists qt wh

instance (Backend b, ToJSONKeyValue a) => ToJSON (GExists b a) where
  toJSON (GExists gTable gWhere) =
    object [ "_table" .= gTable
           , "_where" .= gWhere
           ]

makeLenses ''GExists



----------------------------------------------------------------------------------------------------
-- Boolean expressions in permissions

-- | We don't allow conditions across relationships in permissions: the type we use as the terms in
-- GBoolExp is this one, ColExp, which only contains a FieldName and a JSON Value.
data ColExp
  = ColExp
  { ceCol :: !FieldName
  , ceVal :: !Value
  } deriving (Show, Eq, Data, Generic)
instance NFData ColExp
instance Cacheable ColExp

instance FromJSONKeyValue ColExp where
  parseJSONKeyValue (k, v) = ColExp (FieldName k) <$> parseJSON v

instance ToJSONKeyValue ColExp where
  toJSONKeyValue (ColExp k v) = (getFieldNameTxt k, v)


-- | This @BoolExp@ type is a simple alias for the boolean expressions used in permissions, that
-- uses 'ColExp' as the term in GBoolExp.
newtype BoolExp (b :: BackendType)
  = BoolExp { unBoolExp :: GBoolExp b ColExp }
  deriving newtype (Show, Eq, Generic, NFData, Cacheable, ToJSON, FromJSON)

$(makeWrapped ''BoolExp)

makePrisms ''GBoolExp


-- | Permissions get translated into boolean expressions that are threaded throuhgout the
-- parsers. For the leaf values of those permissions, we use this type, which references but doesn't
-- inline the session variables.
data PartialSQLExp (b :: BackendType)
  = PSESessVar !(SessionVarType b) !SessionVariable
  | PSESQLExp !(SQLExpression b)
  deriving (Generic)
deriving instance (Backend b) => Eq   (PartialSQLExp b)
deriving instance (Backend b) => Data (PartialSQLExp b)
instance (Backend b, NFData    (BooleanOperators b (PartialSQLExp b))) => NFData    (PartialSQLExp b)
instance (Backend b, Cacheable (BooleanOperators b (PartialSQLExp b))) => Hashable  (PartialSQLExp b)
instance (Backend b, Hashable  (BooleanOperators b (PartialSQLExp b))) => Cacheable (PartialSQLExp b)

instance Backend b => ToJSON (PartialSQLExp b) where
  toJSON = \case
    PSESessVar colTy sessVar -> toJSON (colTy, sessVar)
    PSESQLExp e              -> toJSON $ toSQLTxt e

isStaticValue :: PartialSQLExp backend -> Bool
isStaticValue = \case
  PSESessVar _ _ -> False
  PSESQLExp _    -> True

hasStaticExp :: Backend b => OpExpG b (PartialSQLExp b) -> Bool
hasStaticExp = getAny . foldMap (Any . isStaticValue)



----------------------------------------------------------------------------------------------------
-- Boolean expressions in the schema

-- | Operand for cast operator
type CastExp b a = M.HashMap (ScalarType b) [OpExpG b a]

-- | This type represents the boolean operators that can be applied on values of a column. This type
-- only contains the common core, that we expect to be ultimately entirely supported in most if not
-- all backends. Backends can extend this with the @BooleanOperators@ type in @Backend@.
data OpExpG (b :: BackendType) a
  = ACast !(CastExp b a)

  | AEQ !Bool !a
  | ANE !Bool !a
  | AIN  !a
  | ANIN !a
  | AGT  !a
  | ALT  !a
  | AGTE !a
  | ALTE !a

  | ALIKE  !a -- LIKE
  | ANLIKE !a -- NOT LIKE

  -- column comparison operators, the (Maybe (TableName b))
  -- is for setting the root table if there's a comparison
  -- of a relationship column with a column of the root table
  -- it will be set, otherwise it will be Nothing

  | CEQ  !(Column b, Maybe (TableName b))
  | CNE  !(Column b, Maybe (TableName b))
  | CGT  !(Column b, Maybe (TableName b))
  | CLT  !(Column b, Maybe (TableName b))
  | CGTE !(Column b, Maybe (TableName b))
  | CLTE !(Column b, Maybe (TableName b))

  | ANISNULL    -- IS NULL
  | ANISNOTNULL -- IS NOT NULL

  | ABackendSpecific !(BooleanOperators b a)
  deriving (Generic)
deriving instance (Backend b) => Functor     (OpExpG b)
deriving instance (Backend b) => Foldable    (OpExpG b)
deriving instance (Backend b) => Traversable (OpExpG b)
deriving instance (Backend b, Show (BooleanOperators b a), Show a) => Show (OpExpG b a)
deriving instance (Backend b, Eq   (BooleanOperators b a), Eq   a) => Eq (OpExpG b a)
instance (Backend b, NFData    (BooleanOperators b a), NFData    a) => NFData    (OpExpG b a)
instance (Backend b, Cacheable (BooleanOperators b a), Cacheable a) => Cacheable (OpExpG b a)
instance (Backend b, Hashable  (BooleanOperators b a), Hashable  a) => Hashable  (OpExpG b a)

instance (Backend b, ToJSONKeyValue (BooleanOperators b a), ToJSON a) => ToJSONKeyValue (OpExpG b a) where
  toJSONKeyValue = \case
    ACast a            -> ("_cast",    toJSON $ object . map toJSONKeyValue <$> a)

    AEQ _ a            -> ("_eq",      toJSON a)
    ANE _ a            -> ("_ne",      toJSON a)
    AIN a              -> ("_in",      toJSON a)
    ANIN a             -> ("_nin",     toJSON a)
    AGT a              -> ("_gt",      toJSON a)
    ALT a              -> ("_lt",      toJSON a)
    AGTE a             -> ("_gte",     toJSON a)
    ALTE a             -> ("_lte",     toJSON a)

    ALIKE a            -> ("_like",    toJSON a)
    ANLIKE a           -> ("_nlike",   toJSON a)

    CEQ a              -> ("_ceq",     toJSON a)
    CNE a              -> ("_cne",     toJSON a)
    CGT a              -> ("_cgt",     toJSON a)
    CLT a              -> ("_clt",     toJSON a)
    CGTE a             -> ("_cgte",    toJSON a)
    CLTE a             -> ("_clte",    toJSON a)

    ANISNULL           -> ("_is_null", toJSON True)
    ANISNOTNULL        -> ("_is_null", toJSON False)

    ABackendSpecific b -> toJSONKeyValue b

opExpDepCol :: OpExpG backend a -> Maybe (Column backend, Maybe (TableName backend))
opExpDepCol = \case
  CEQ c  -> Just c
  CNE c  -> Just c
  CGT c  -> Just c
  CLT c  -> Just c
  CGTE c -> Just c
  CLTE c -> Just c
  _      -> Nothing


-- | This type is used for boolean terms in GBoolExp in the schema; there are two kinds boolean
-- terms:
--   - operators on a column of the current table, using the 'OpExpG' kind of operators
--   - arbitrary expressions on columns of tables in relationships (in the same source)
--
-- This type is parametric over the type of leaf values, the values on which we operate.
data AnnBoolExpFld (b :: BackendType) a
  = AVCol !(ColumnInfo b) ![OpExpG b a]
  | AVRel !(RelInfo b) !(AnnBoolExp b a)
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Eq (BooleanOperators b a), Eq a) => Eq (AnnBoolExpFld b a)
instance (Backend b, NFData    (BooleanOperators b a), NFData    a) => NFData    (AnnBoolExpFld b a)
instance (Backend b, Cacheable (BooleanOperators b a), Cacheable a) => Cacheable (AnnBoolExpFld b a)
instance (Backend b, Hashable  (BooleanOperators b a), Hashable  a) => Hashable  (AnnBoolExpFld b a)

instance (Backend b, ToJSONKeyValue (BooleanOperators b a), ToJSON a) => ToJSONKeyValue (AnnBoolExpFld b a) where
  toJSONKeyValue = \case
    AVCol pci opExps ->
      ( toTxt $ pgiColumn pci
      , toJSON (pci, object . pure . toJSONKeyValue <$> opExps)
      )
    AVRel ri relBoolExp ->
      ( relNameToTxt $ riName ri
      , toJSON (ri, toJSON relBoolExp)
      )

-- | A simple alias for the kind of boolean expressions used in the schema, that ties together
-- 'GBoolExp', 'OpExpG', and 'AnnBoolExpFld'.
type AnnBoolExp b a = GBoolExp b (AnnBoolExpFld b a)


-- Type aliases for common use cases:
type AnnBoolExpFldSQL     b = AnnBoolExpFld b (SQLExpression b)
type AnnBoolExpSQL        b = AnnBoolExp    b (SQLExpression b)
type AnnBoolExpPartialSQL b = AnnBoolExp    b (PartialSQLExp b)


annBoolExpTrue :: AnnBoolExp backend a
annBoolExpTrue = gBoolExpTrue

andAnnBoolExps :: AnnBoolExp backend a -> AnnBoolExp backend a -> AnnBoolExp backend a
andAnnBoolExps l r =
  BoolAnd [l, r]


-- Traversal functions
fmapAnnBoolExp
  :: Backend backend
  => (a -> b)
  -> AnnBoolExp backend a
  -> AnnBoolExp backend b
fmapAnnBoolExp f =
  runIdentity . traverseAnnBoolExp (pure . f)

traverseAnnBoolExpFld
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnBoolExpFld backend a
  -> f (AnnBoolExpFld backend b)
traverseAnnBoolExpFld f = \case
  AVCol pgColInfo opExps ->
    AVCol pgColInfo <$> traverse (traverse f) opExps
  AVRel relInfo annBoolExp ->
    AVRel relInfo <$> traverseAnnBoolExp f annBoolExp

traverseAnnBoolExp
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnBoolExp backend a
  -> f (AnnBoolExp backend b)
traverseAnnBoolExp f = traverse (traverseAnnBoolExpFld f)



----------------------------------------------------------------------------------------------------
-- Operands for specific operators

-- Arguably, most of those should be moved elsewhere, since not all of the corresponding operators
-- are part of the common core of operators.

-- | Operand for STDWithin opoerator
data DWithinGeomOp a =
  DWithinGeomOp
  { dwgeomDistance :: !a
  , dwgeomFrom     :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)
instance (NFData    a) => NFData    (DWithinGeomOp a)
instance (Cacheable a) => Cacheable (DWithinGeomOp a)
instance (Hashable  a) => Hashable  (DWithinGeomOp a)
$(deriveJSON hasuraJSON ''DWithinGeomOp)

-- | Operand for STDWithin opoerator
data DWithinGeogOp a =
  DWithinGeogOp
  { dwgeogDistance    :: !a
  , dwgeogFrom        :: !a
  , dwgeogUseSpheroid :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)
instance (NFData    a) => NFData    (DWithinGeogOp a)
instance (Cacheable a) => Cacheable (DWithinGeogOp a)
instance (Hashable  a) => Hashable  (DWithinGeogOp a)
$(deriveJSON hasuraJSON ''DWithinGeogOp)

-- | Operand for STIntersect
data STIntersectsNbandGeommin a =
  STIntersectsNbandGeommin
  { singNband   :: !a
  , singGeommin :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)
instance (NFData    a) => NFData    (STIntersectsNbandGeommin a)
instance (Cacheable a) => Cacheable (STIntersectsNbandGeommin a)
instance (Hashable  a) => Hashable  (STIntersectsNbandGeommin a)
$(deriveJSON hasuraJSON ''STIntersectsNbandGeommin)

-- | Operand for STIntersect
data STIntersectsGeomminNband a =
  STIntersectsGeomminNband
  { signGeommin :: !a
  , signNband   :: !(Maybe a)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)
instance (NFData    a) => NFData    (STIntersectsGeomminNband a)
instance (Cacheable a) => Cacheable (STIntersectsGeomminNband a)
instance (Hashable  a) => Hashable  (STIntersectsGeomminNband a)
$(deriveJSON hasuraJSON ''STIntersectsGeomminNband)



----------------------------------------------------------------------------------------------------
-- Miscellaneous

-- | This is a simple newtype over AnnBoolExpFld. At time of writing, I do not know why we want
-- this, and why it exists. It might be a relic of a needed differentiation, now lost?
-- TODO: can this be removed?
newtype AnnColumnCaseBoolExpField (b :: BackendType) a
  = AnnColumnCaseBoolExpField { _accColCaseBoolExpField :: (AnnBoolExpFld b a)}
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Eq (BooleanOperators b a), Eq a) => Eq (AnnColumnCaseBoolExpField b a)
instance (Backend b, NFData    (BooleanOperators b a), NFData    a) => NFData    (AnnColumnCaseBoolExpField b a)
instance (Backend b, Cacheable (BooleanOperators b a), Cacheable a) => Cacheable (AnnColumnCaseBoolExpField b a)
instance (Backend b, Hashable  (BooleanOperators b a), Hashable  a) => Hashable  (AnnColumnCaseBoolExpField b a)

instance (Backend b, ToJSONKeyValue (BooleanOperators b a), ToJSON a) => ToJSONKeyValue (AnnColumnCaseBoolExpField b a) where
  toJSONKeyValue = toJSONKeyValue . _accColCaseBoolExpField


-- | Similar to AnnBoolExp, this type alias ties together
-- 'GBoolExp', 'OpExpG', and 'AnnColumnCaseBoolExpFld'.
type AnnColumnCaseBoolExp b a = GBoolExp b (AnnColumnCaseBoolExpField b a)


-- traversal functions
fmapAnnColumnCaseBoolExp
  :: Backend backend
  => (a -> b)
  -> AnnColumnCaseBoolExp backend a
  -> AnnColumnCaseBoolExp backend b
fmapAnnColumnCaseBoolExp f =
  runIdentity . traverseAnnColumnCaseBoolExp (pure . f)

traverseAnnColumnCaseBoolExp
  :: (Applicative f, Backend backend)
  => (a -> f b)
  -> AnnColumnCaseBoolExp backend a
  -> f (AnnColumnCaseBoolExp backend b)
traverseAnnColumnCaseBoolExp f = traverse traverseColCaseBoolExp
  where
    traverseColCaseBoolExp (AnnColumnCaseBoolExpField annBoolExpField) =
      AnnColumnCaseBoolExpField <$> traverseAnnBoolExpFld f annBoolExpField


-- misc type aliases
type AnnColumnCaseBoolExpPartialSQL b = AnnColumnCaseBoolExp b (PartialSQLExp b)

type PreSetColsG b v = M.HashMap (Column b) v
type PreSetColsPartial b = M.HashMap (Column b) (PartialSQLExp b)
