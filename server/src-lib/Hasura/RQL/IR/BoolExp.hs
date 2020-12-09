{-# LANGUAGE UndecidableInstances #-}
module Hasura.RQL.IR.BoolExp
       ( BoolExp(..)
       , ColExp(..)
       , GBoolExp(..)
       , gBoolExpTrue
       , gBoolExpToJSON
       , parseGBoolExp
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
       , traverseAnnBoolExp
       , fmapAnnBoolExp
       , annBoolExpTrue
       , andAnnBoolExps

       , AnnBoolExpFldSQL
       , AnnBoolExpSQL
       , PartialSQLExp(..)
       , isStaticValue
       , AnnBoolExpFldPartialSQL
       , AnnBoolExpPartialSQL

       , PreSetColsG
       , PreSetColsPartial
       ) where

import           Hasura.Prelude

import qualified Data.Aeson.Types                   as J
import qualified Data.HashMap.Strict                as M

import           Control.Lens.Plated
import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Internal
import           Data.Aeson.TH
import           Data.Typeable
import           Data.Text.Extended

import           Hasura.Incremental                 (Cacheable)
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.SQL.Backend
import           Hasura.SQL.Types
import           Hasura.Session


data ColExp
  = ColExp
  { ceCol :: !FieldName
  , ceVal :: !Value
  } deriving (Show, Eq, Data, Generic)
instance NFData ColExp
instance Cacheable ColExp


data GExists (b :: BackendType) a
  = GExists
  { _geTable :: !(TableName b)
  , _geWhere :: !(GBoolExp b a)
  } deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Show a) => Show (GExists b a)
deriving instance (Backend b, Eq a) => Eq (GExists b a)
deriving instance (Backend b, Typeable a, Data a) => Data (GExists b a)
instance (Backend b, NFData a) => NFData (GExists b a)
instance (Backend b, Data a) => Plated (GExists b a)
instance (Backend b, Cacheable a) => Cacheable (GExists b a)
instance (Backend b, Hashable a) => Hashable (GExists b a)

gExistsToJSON :: (a -> (Text, Value)) -> GExists 'Postgres a -> Value
gExistsToJSON f (GExists qt wh) =
  object [ "_table" .= qt
         , "_where" .= gBoolExpToJSON f wh
         ]

parseGExists
  :: ((Text, Value) -> J.Parser a) -> Value -> J.Parser (GExists 'Postgres a)
parseGExists f = \case
  Object o -> do
    qt <- o .: "_table"
    wh <- o .: "_where"
    GExists qt <$> parseGBoolExp f wh
  _ -> fail "expecting an Object for _exists expression"


data GBoolExp (b :: BackendType) a
  = BoolAnd ![GBoolExp b a]
  | BoolOr  ![GBoolExp b a]
  | BoolNot !(GBoolExp b a)
  | BoolExists !(GExists b a)
  | BoolFld !a
  deriving (Show, Eq, Functor, Foldable, Traversable, Data, Generic)
instance (Backend b, NFData a) => NFData (GBoolExp b a)
instance (Backend b, Data a) => Plated (GBoolExp b a)
instance (Backend b, Cacheable a) => Cacheable (GBoolExp b a)
instance (Backend b, Hashable a) => Hashable (GBoolExp b a)

gBoolExpTrue :: GBoolExp b a
gBoolExpTrue = BoolAnd []

gBoolExpToJSON :: (a -> (Text, Value)) -> GBoolExp 'Postgres a -> Value
gBoolExpToJSON f be = case be of
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
      BoolAnd bExps      -> "_and" .= map (gBoolExpToJSON f) bExps
      BoolOr bExps       -> "_or" .= map (gBoolExpToJSON f) bExps
      BoolNot bExp       -> "_not" .= gBoolExpToJSON f bExp
      BoolExists bExists -> "_exists" .= gExistsToJSON f bExists
      BoolFld a          ->  f a

parseGBoolExp
  :: ((Text, Value) -> J.Parser a) -> Value -> J.Parser (GBoolExp 'Postgres a)
parseGBoolExp f = \case
  Object o -> do
    boolExps <- forM (M.toList o) $ \(k, v) -> if
      | k == "$or"     -> BoolOr  <$> parseGBoolExpL v <?> Key k
      | k == "_or"     -> BoolOr  <$> parseGBoolExpL v <?> Key k
      | k == "$and"    -> BoolAnd <$> parseGBoolExpL v <?> Key k
      | k == "_and"    -> BoolAnd <$> parseGBoolExpL v <?> Key k
      | k == "$not"    -> BoolNot <$> parseGBoolExp f v <?> Key k
      | k == "_not"    -> BoolNot <$> parseGBoolExp f v <?> Key k
      | k == "$exists" -> BoolExists <$> parseGExists f v <?> Key k
      | k == "_exists" -> BoolExists <$> parseGExists f v <?> Key k
      | otherwise      -> BoolFld <$> f (k, v)
    return $ BoolAnd boolExps
  _ -> fail "expecting an Object for boolean exp"
  where
    parseGBoolExpL v =
      parseJSON v >>= mapM (parseGBoolExp f)


newtype BoolExp (b :: BackendType)
  = BoolExp { unBoolExp :: GBoolExp b ColExp }
  deriving (Show, Eq, Generic, NFData, Cacheable)

$(makeWrapped ''BoolExp)

instance ToJSON (BoolExp 'Postgres) where
  toJSON (BoolExp gBoolExp) =
    gBoolExpToJSON f gBoolExp
    where
      f (ColExp k v) =
        (getFieldNameTxt k,  v)

instance FromJSON (BoolExp 'Postgres) where
  parseJSON =
    fmap BoolExp . parseGBoolExp f
    where
      f (k, v) = ColExp (FieldName k) <$> parseJSON v


data DWithinGeomOp a =
  DWithinGeomOp
  { dwgeomDistance :: !a
  , dwgeomFrom     :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)
instance (NFData a) => NFData (DWithinGeomOp a)
instance (Cacheable a) => Cacheable (DWithinGeomOp a)
instance (Hashable a) => Hashable (DWithinGeomOp a)
$(deriveJSON (aesonDrop 6 snakeCase) ''DWithinGeomOp)

data DWithinGeogOp a =
  DWithinGeogOp
  { dwgeogDistance    :: !a
  , dwgeogFrom        :: !a
  , dwgeogUseSpheroid :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)
instance (NFData a) => NFData (DWithinGeogOp a)
instance (Cacheable a) => Cacheable (DWithinGeogOp a)
instance (Hashable a) => Hashable (DWithinGeogOp a)
$(deriveJSON (aesonDrop 6 snakeCase) ''DWithinGeogOp)

data STIntersectsNbandGeommin a =
  STIntersectsNbandGeommin
  { singNband   :: !a
  , singGeommin :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)
instance (NFData a) => NFData (STIntersectsNbandGeommin a)
instance (Cacheable a) => Cacheable (STIntersectsNbandGeommin a)
instance (Hashable a) => Hashable (STIntersectsNbandGeommin a)
$(deriveJSON (aesonDrop 4 snakeCase) ''STIntersectsNbandGeommin)

data STIntersectsGeomminNband a =
  STIntersectsGeomminNband
  { signGeommin :: !a
  , signNband   :: !(Maybe a)
  } deriving (Show, Eq, Functor, Foldable, Traversable, Generic, Data)
instance (NFData a) => NFData (STIntersectsGeomminNband a)
instance (Cacheable a) => Cacheable (STIntersectsGeomminNband a)
instance (Hashable a) => Hashable (STIntersectsGeomminNband a)
$(deriveJSON (aesonDrop 4 snakeCase) ''STIntersectsGeomminNband)

type CastExp b a = M.HashMap (ScalarType b) [OpExpG b a]

data OpExpG (b :: BackendType) a
  = ACast !(CastExp b a)

  | AEQ !Bool !a
  | ANE !Bool !a

  | AIN  !a
  | ANIN !a

  | AGT !a
  | ALT !a
  | AGTE !a
  | ALTE !a

  | ALIKE !a -- LIKE
  | ANLIKE !a -- NOT LIKE

  | AILIKE (XAILIKE b) !a -- ILIKE, case insensitive
  | ANILIKE (XANILIKE b) !a-- NOT ILIKE, case insensitive

  | ASIMILAR !a -- similar, regex
  | ANSIMILAR !a-- not similar, regex

  -- Now that in the RQL code we've started to take a "trees that grow"
  -- approach (see PR #6003), we may eventually want to move these
  -- recently added constructors, which correspond to newly supported
  -- Postgres operators, to the backend-specific extensions of this type.
  | AREGEX !a -- match POSIX case sensitive, regex
  | AIREGEX !a -- match POSIX case insensitive, regex
  | ANREGEX !a -- dont match POSIX case sensitive, regex
  | ANIREGEX !a -- dont match POSIX case insensitive, regex

  | AContains !a
  | AContainedIn !a
  | AHasKey !a
  | AHasKeysAny !a
  | AHasKeysAll !a

  | ASTContains !a
  | ASTCrosses !a
  | ASTDWithinGeom !(DWithinGeomOp a)
  | ASTDWithinGeog !(DWithinGeogOp a)
  | ASTEquals !a
  | ASTIntersects !a
  | ASTOverlaps !a
  | ASTTouches !a
  | ASTWithin !a

  | ASTIntersectsRast !a
  | ASTIntersectsGeomNband !(STIntersectsGeomminNband a)
  | ASTIntersectsNbandGeom !(STIntersectsNbandGeommin a)

  | ANISNULL -- IS NULL
  | ANISNOTNULL -- IS NOT NULL

  | CEQ !(Column b)
  | CNE !(Column b)
  | CGT !(Column b)
  | CLT !(Column b)
  | CGTE !(Column b)
  | CLTE !(Column b)
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Eq a) => Eq (OpExpG b a)
instance (Backend b, NFData a) => NFData (OpExpG b a)
instance (Backend b, Cacheable a) => Cacheable (OpExpG b a)
instance (Backend b, Hashable a) => Hashable (OpExpG b a)


opExpDepCol :: OpExpG backend a -> Maybe (Column backend)
opExpDepCol = \case
  CEQ c  -> Just c
  CNE c  -> Just c
  CGT c  -> Just c
  CLT c  -> Just c
  CGTE c -> Just c
  CLTE c -> Just c
  _      -> Nothing

opExpToJPair :: (a -> Value) -> OpExpG 'Postgres a -> (Text, Value)
opExpToJPair f = \case
  ACast a                  -> ("_cast", toJSON $ M.map opExpsToJSON a)

  AEQ _ a                  -> ("_eq", f a)
  ANE _ a                  -> ("_ne", f a)

  AIN a                    -> ("_in", f a)
  ANIN a                   -> ("_nin", f a)

  AGT a                    -> ("_gt", f a)
  ALT a                    -> ("_lt", f a)
  AGTE a                   -> ("_gte", f a)
  ALTE a                   -> ("_lte", f a)

  ALIKE a                  -> ("_like", f a)
  ANLIKE a                 -> ("_nlike", f a)

  AILIKE _ a               -> ("_ilike", f a)
  ANILIKE _ a              -> ("_nilike", f a)

  ASIMILAR a               -> ("_similar", f a)
  ANSIMILAR a              -> ("_nsimilar", f a)

  AREGEX a                 -> ("_regex", f a)
  AIREGEX a                -> ("_iregex", f a)
  ANREGEX a                -> ("_nregex", f a)
  ANIREGEX a               -> ("_niregex", f a)

  AContains a              -> ("_contains", f a)
  AContainedIn a           -> ("_contained_in", f a)
  AHasKey a                -> ("_has_key", f a)
  AHasKeysAny a            -> ("_has_keys_any", f a)
  AHasKeysAll a            -> ("_has_keys_all", f a)

  ASTContains a            -> ("_st_contains", f a)
  ASTCrosses a             -> ("_st_crosses", f a)
  ASTDWithinGeom o         -> ("_st_d_within", toJSON $ f <$> o)
  ASTDWithinGeog o         -> ("_st_d_within", toJSON $ f <$> o)
  ASTEquals a              -> ("_st_equals", f a)
  ASTIntersects a          -> ("_st_intersects", f a)
  ASTOverlaps a            -> ("_st_overlaps", f a)
  ASTTouches a             -> ("_st_touches", f a)
  ASTWithin a              -> ("_st_within", f a)

  ASTIntersectsRast a      -> ("_st_intersects_rast", f a)
  ASTIntersectsNbandGeom a -> ("_st_intersects_nband_geom", toJSON $ f <$> a)
  ASTIntersectsGeomNband a -> ("_st_intersects_geom_nband", toJSON $ f <$> a)

  ANISNULL                 -> ("_is_null", toJSON True)
  ANISNOTNULL              -> ("_is_null", toJSON False)

  CEQ a                    -> ("_ceq", toJSON a)
  CNE a                    -> ("_cne", toJSON a)
  CGT a                    -> ("_cgt", toJSON a)
  CLT a                    -> ("_clt", toJSON a)
  CGTE a                   -> ("_cgte", toJSON a)
  CLTE a                   -> ("_clte", toJSON a)
  where
    opExpsToJSON = object . map (opExpToJPair f)

data AnnBoolExpFld (b :: BackendType) a
  = AVCol !(ColumnInfo b) ![OpExpG b a]
  | AVRel !(RelInfo b) !(AnnBoolExp b a)
  deriving (Functor, Foldable, Traversable, Generic)
deriving instance (Backend b, Eq (ColumnInfo b), Eq a) => Eq (AnnBoolExpFld b a)
instance (Backend b, NFData (ColumnInfo b), NFData a) => NFData (AnnBoolExpFld b a)
instance (Backend b, Cacheable (ColumnInfo b), Cacheable a) => Cacheable (AnnBoolExpFld b a)
instance (Backend b, Hashable (ColumnInfo b), Hashable a) => Hashable (AnnBoolExpFld b a)

type AnnBoolExp b a
  = GBoolExp b (AnnBoolExpFld b a)

traverseAnnBoolExp
  :: (Applicative f)
  => (a -> f b)
  -> AnnBoolExp backend a
  -> f (AnnBoolExp backend b)
traverseAnnBoolExp f =
  traverse $ \case
   AVCol pgColInfo opExps ->
     AVCol pgColInfo <$> traverse (traverse f) opExps
   AVRel relInfo annBoolExp ->
     AVRel relInfo <$> traverseAnnBoolExp f annBoolExp

fmapAnnBoolExp
  :: (a -> b)
  -> AnnBoolExp backend a
  -> AnnBoolExp backend b
fmapAnnBoolExp f =
  runIdentity . traverseAnnBoolExp (pure . f)

annBoolExpTrue :: AnnBoolExp backend a
annBoolExpTrue = gBoolExpTrue

andAnnBoolExps :: AnnBoolExp backend a -> AnnBoolExp backend a -> AnnBoolExp backend a
andAnnBoolExps l r =
  BoolAnd [l, r]

type AnnBoolExpFldSQL b = AnnBoolExpFld b (SQLExpression b)
type AnnBoolExpSQL    b = AnnBoolExp    b (SQLExpression b)

type AnnBoolExpFldPartialSQL b = AnnBoolExpFld b (PartialSQLExp b)
type AnnBoolExpPartialSQL b = AnnBoolExp b (PartialSQLExp b)

type PreSetColsG b v = M.HashMap (Column b) v
type PreSetColsPartial b = M.HashMap (Column b) (PartialSQLExp b)

-- doesn't resolve the session variable
data PartialSQLExp (b :: BackendType)
  = PSESessVar !(SessionVarType b) !SessionVariable
  | PSESQLExp !(SQLExpression b)
  deriving (Generic)
deriving instance Backend b => Eq (PartialSQLExp b)
deriving instance Backend b => Data (PartialSQLExp b)
instance Backend b => NFData (PartialSQLExp b)
instance Backend b => Cacheable (PartialSQLExp b)

instance ToJSON (PartialSQLExp 'Postgres) where
  toJSON = \case
    PSESessVar colTy sessVar -> toJSON (colTy, sessVar)
    PSESQLExp e              -> toJSON $ toSQLTxt e

instance ToJSON (AnnBoolExpPartialSQL 'Postgres) where
  toJSON = gBoolExpToJSON f
    where
      f annFld = case annFld of
        AVCol pci opExps ->
          ( toTxt $ pgiColumn pci
          , toJSON (pci, map opExpSToJSON opExps)
          )
        AVRel ri relBoolExp ->
          ( relNameToTxt $ riName ri
          , toJSON (ri, toJSON relBoolExp)
          )
      opExpSToJSON :: OpExpG 'Postgres (PartialSQLExp 'Postgres) -> Value
      opExpSToJSON =
        object . pure . opExpToJPair toJSON

isStaticValue :: PartialSQLExp backend -> Bool
isStaticValue = \case
  PSESessVar _ _ -> False
  PSESQLExp _    -> True

makeLenses ''GExists
makePrisms ''GBoolExp
