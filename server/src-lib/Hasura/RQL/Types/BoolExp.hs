module Hasura.RQL.Types.BoolExp
       ( GBoolExp(..)
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
       , mkTypedSessionVar
       , isStaticValue
       , AnnBoolExpFldPartialSQL
       , AnnBoolExpPartialSQL

       , PreSetCols
       , PreSetColsPartial
       ) where

import           Hasura.Incremental         (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Types.Column
import           Hasura.RQL.Types.Common
import           Hasura.Session
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML             as S

import           Control.Lens.Plated
import           Control.Lens.TH
import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Internal
import           Data.Aeson.TH
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

import qualified Data.Aeson.Types           as J
import qualified Data.HashMap.Strict        as M

data GExists a
  = GExists
  { _geTable :: !QualifiedTable
  , _geWhere :: !(GBoolExp a)
  } deriving (Show, Eq, Lift, Functor, Foldable, Traversable, Data, Generic)
instance (NFData a) => NFData (GExists a)
instance (Data a) => Plated (GExists a)
instance (Cacheable a) => Cacheable (GExists a)
instance (Hashable a) => Hashable (GExists a)

gExistsToJSON :: (a -> (Text, Value)) -> GExists a -> Value
gExistsToJSON f (GExists qt wh) =
  object [ "_table" .= qt
         , "_where" .= gBoolExpToJSON f wh
         ]

parseGExists
  :: ((Text, Value) -> J.Parser a) -> Value -> J.Parser (GExists a)
parseGExists f = \case
  Object o -> do
    qt <- o .: "_table"
    wh <- o .: "_where"
    GExists qt <$> parseGBoolExp f wh
  _ -> fail "expecting an Object for _exists expression"

data GBoolExp a
  = BoolAnd ![GBoolExp a]
  | BoolOr  ![GBoolExp a]
  | BoolNot !(GBoolExp a)
  | BoolExists !(GExists a)
  | BoolFld !a
  deriving (Show, Eq, Lift, Functor, Foldable, Traversable, Data, Generic)
instance (NFData a) => NFData (GBoolExp a)
instance (Data a) => Plated (GBoolExp a)
instance (Cacheable a) => Cacheable (GBoolExp a)
instance (Hashable a) => Hashable (GBoolExp a)

gBoolExpTrue :: GBoolExp a
gBoolExpTrue = BoolAnd []

gBoolExpToJSON :: (a -> (Text, Value)) -> GBoolExp a -> Value
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
      BoolAnd bExps -> "_and" .= map (gBoolExpToJSON f) bExps
      BoolOr bExps  -> "_or" .= map (gBoolExpToJSON f) bExps
      BoolNot bExp  -> "_not" .= gBoolExpToJSON f bExp
      BoolExists bExists -> "_exists" .= gExistsToJSON f bExists
      BoolFld a     ->  f a


parseGBoolExp
  :: ((Text, Value) -> J.Parser a) -> Value -> J.Parser (GBoolExp a)
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

type CastExp a = M.HashMap PGScalarType [OpExpG a]

data OpExpG a
  = ACast !(CastExp a)

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

  | AILIKE !a -- ILIKE, case insensitive
  | ANILIKE !a-- NOT ILIKE, case insensitive

  | ASIMILAR !a -- similar, regex
  | ANSIMILAR !a-- not similar, regex

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

  | CEQ !PGCol
  | CNE !PGCol
  | CGT !PGCol
  | CLT !PGCol
  | CGTE !PGCol
  | CLTE !PGCol
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic, Data)
instance (NFData a) => NFData (OpExpG a)
instance (Cacheable a) => Cacheable (OpExpG a)
instance (Hashable a) => Hashable (OpExpG a)

opExpDepCol :: OpExpG a -> Maybe PGCol
opExpDepCol = \case
  CEQ c  -> Just c
  CNE c  -> Just c
  CGT c  -> Just c
  CLT c  -> Just c
  CGTE c -> Just c
  CLTE c -> Just c
  _      -> Nothing

opExpToJPair :: (a -> Value) -> OpExpG a -> (Text, Value)
opExpToJPair f = \case
  ACast a        -> ("_cast", toJSON $ M.map opExpsToJSON a)

  AEQ _ a          -> ("_eq", f a)
  ANE _ a          -> ("_ne", f a)

  AIN a          -> ("_in", f a)
  ANIN a         -> ("_nin", f a)

  AGT a          -> ("_gt", f a)
  ALT a          -> ("_lt", f a)
  AGTE a         -> ("_gte", f a)
  ALTE a         -> ("_lte", f a)

  ALIKE a        -> ("_like", f a)
  ANLIKE a       -> ("_nlike", f a)

  AILIKE a       -> ("_ilike", f a)
  ANILIKE a      -> ("_nilike", f a)

  ASIMILAR a     -> ("_similar", f a)
  ANSIMILAR a    -> ("_nsimilar", f a)

  AContains a    -> ("_contains", f a)
  AContainedIn a -> ("_contained_in", f a)
  AHasKey a      -> ("_has_key", f a)
  AHasKeysAny a  -> ("_has_keys_any", f a)
  AHasKeysAll a  -> ("_has_keys_all", f a)

  ASTContains a    -> ("_st_contains", f a)
  ASTCrosses a     -> ("_st_crosses", f a)
  ASTDWithinGeom o -> ("_st_d_within", toJSON $ f <$> o)
  ASTDWithinGeog o -> ("_st_d_within", toJSON $ f <$> o)
  ASTEquals a      -> ("_st_equals", f a)
  ASTIntersects a  -> ("_st_intersects", f a)
  ASTOverlaps a    -> ("_st_overlaps", f a)
  ASTTouches a     -> ("_st_touches", f a)
  ASTWithin a      -> ("_st_within", f a)

  ASTIntersectsRast a      -> ("_st_intersects_rast", f a)
  ASTIntersectsNbandGeom a -> ("_st_intersects_nband_geom", toJSON $ f <$> a)
  ASTIntersectsGeomNband a -> ("_st_intersects_geom_nband", toJSON $ f <$> a)

  ANISNULL       -> ("_is_null", toJSON True)
  ANISNOTNULL    -> ("_is_null", toJSON False)

  CEQ a          -> ("_ceq", toJSON a)
  CNE a          -> ("_cne", toJSON a)
  CGT a          -> ("_cgt", toJSON a)
  CLT a          -> ("_clt", toJSON a)
  CGTE a         -> ("_cgte", toJSON a)
  CLTE a         -> ("_clte", toJSON a)
  where
    opExpsToJSON = object . map (opExpToJPair f)

data AnnBoolExpFld a
  = AVCol !PGColumnInfo ![OpExpG a]
  | AVRel !RelInfo !(AnnBoolExp a)
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
instance (NFData a) => NFData (AnnBoolExpFld a)
instance (Cacheable a) => Cacheable (AnnBoolExpFld a)
instance (Hashable a) => Hashable (AnnBoolExpFld a)

type AnnBoolExp a
  = GBoolExp (AnnBoolExpFld a)

traverseAnnBoolExp
  :: (Applicative f)
  => (a -> f b)
  -> AnnBoolExp a
  -> f (AnnBoolExp b)
traverseAnnBoolExp f =
  traverse $ \case
   AVCol pgColInfo opExps ->
     AVCol pgColInfo <$> traverse (traverse f) opExps
   AVRel relInfo annBoolExp ->
     AVRel relInfo <$> traverseAnnBoolExp f annBoolExp

fmapAnnBoolExp
  :: (a -> b)
  -> AnnBoolExp a
  -> AnnBoolExp b
fmapAnnBoolExp f =
  runIdentity . traverseAnnBoolExp (pure . f)

annBoolExpTrue :: AnnBoolExp a
annBoolExpTrue = gBoolExpTrue

andAnnBoolExps :: AnnBoolExp a -> AnnBoolExp a -> AnnBoolExp a
andAnnBoolExps l r =
  BoolAnd [l, r]

type AnnBoolExpFldSQL = AnnBoolExpFld S.SQLExp
type AnnBoolExpSQL = AnnBoolExp S.SQLExp

type AnnBoolExpFldPartialSQL = AnnBoolExpFld PartialSQLExp
type AnnBoolExpPartialSQL = AnnBoolExp PartialSQLExp

type PreSetColsPartial = M.HashMap PGCol PartialSQLExp
type PreSetCols = M.HashMap PGCol S.SQLExp

-- doesn't resolve the session variable
data PartialSQLExp
  = PSESessVar !(PGType PGScalarType) !SessionVariable
  | PSESQLExp !S.SQLExp
  deriving (Show, Eq, Generic, Data)
instance NFData PartialSQLExp
instance Cacheable PartialSQLExp

mkTypedSessionVar :: PGType PGColumnType -> SessionVariable -> PartialSQLExp
mkTypedSessionVar columnType =
  PSESessVar (unsafePGColumnToRepresentation <$> columnType)

instance ToJSON PartialSQLExp where
  toJSON = \case
    PSESessVar colTy sessVar -> toJSON (colTy, sessVar)
    PSESQLExp e -> toJSON $ toSQLTxt e

instance ToJSON AnnBoolExpPartialSQL where
  toJSON = gBoolExpToJSON f
    where
      f annFld = case annFld of
        AVCol pci opExps ->
          ( getPGColTxt $ pgiColumn pci
          , toJSON (pci, map opExpSToJSON opExps)
          )
        AVRel ri relBoolExp ->
          ( relNameToTxt $ riName ri
          , toJSON (ri, toJSON relBoolExp)
          )
      opExpSToJSON :: OpExpG PartialSQLExp -> Value
      opExpSToJSON =
        object . pure . opExpToJPair toJSON

isStaticValue :: PartialSQLExp -> Bool
isStaticValue = \case
  PSESessVar _ _ -> False
  PSESQLExp _    -> True

makeLenses ''GExists
makePrisms ''GBoolExp
