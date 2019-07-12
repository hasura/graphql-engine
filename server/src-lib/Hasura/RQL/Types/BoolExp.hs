module Hasura.RQL.Types.BoolExp
       ( GBoolExp(..)
       , gBoolExpTrue
       , gBoolExpToJSON
       , parseGBoolExp

       , DWithinGeomOp(..)
       , DWithinGeogOp(..)
       , OpExpG(..)
       , opExpDepCol

       , AnnBoolExpFld(..)
       , AnnBoolExp
       , traverseAnnBoolExp
       , fmapAnnBoolExp
       , annBoolExpTrue
       , andAnnBoolExps

       , AnnBoolExpFldSQL
       , AnnBoolExpSQL
       , PartialSQLExp(..)
       , AnnBoolExpFldPartialSQL
       , AnnBoolExpPartialSQL

       , PreSetCols
       , PreSetColsPartial
       , foldBoolExp
       ) where

import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Permission
import qualified Hasura.SQL.DML              as S
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.Internal
import           Data.Aeson.TH
import qualified Data.Aeson.Types            as J
import qualified Data.HashMap.Strict         as M
import           Instances.TH.Lift           ()
import           Language.Haskell.TH.Syntax  (Lift)

data GBoolExp a
  = BoolAnd ![GBoolExp a]
  | BoolOr  ![GBoolExp a]
  | BoolNot !(GBoolExp a)
  | BoolFld !a
  deriving (Show, Eq, Lift, Functor, Foldable, Traversable)

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
      BoolFld a     ->  f a


parseGBoolExp
  :: ((Text, Value) -> J.Parser a) -> Value -> J.Parser (GBoolExp a)
parseGBoolExp f = \case
  Object o -> do
    boolExps <- forM (M.toList o) $ \(k, v) -> if
      | k == "$or"  -> BoolOr  <$> parseGBoolExpL v <?> Key k
      | k == "_or"  -> BoolOr  <$> parseGBoolExpL v <?> Key k
      | k == "$and" -> BoolAnd <$> parseGBoolExpL v <?> Key k
      | k == "_and" -> BoolAnd <$> parseGBoolExpL v <?> Key k
      | k == "$not" -> BoolNot <$> parseGBoolExp f v <?> Key k
      | k == "_not" -> BoolNot <$> parseGBoolExp f v <?> Key k
      | otherwise   -> BoolFld <$> f (k, v)
    return $ BoolAnd boolExps
  _ -> fail "expecting an Object for boolean exp"
  where
    parseGBoolExpL v =
      parseJSON v >>= mapM (parseGBoolExp f)

foldBoolExp :: (Monad m)
            => (a -> m S.BoolExp)
            -> GBoolExp a
            -> m S.BoolExp
foldBoolExp f (BoolAnd bes) = do
  sqlBExps <- mapM (foldBoolExp f) bes
  return $ foldr (S.BEBin S.AndOp) (S.BELit True) sqlBExps
foldBoolExp f (BoolOr bes)  = do
  sqlBExps <- mapM (foldBoolExp f) bes
  return $ foldr (S.BEBin S.OrOp) (S.BELit False) sqlBExps
foldBoolExp f (BoolNot notExp) =
  S.BENot <$> foldBoolExp f notExp
foldBoolExp f (BoolFld ce)  =
  f ce

data DWithinGeomOp a =
  DWithinGeomOp
  { dwgeomDistance :: !a
  , dwgeomFrom     :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable)
$(deriveJSON (aesonDrop 6 snakeCase) ''DWithinGeomOp)

data DWithinGeogOp a =
  DWithinGeogOp
  { dwgeogDistance    :: !a
  , dwgeogFrom        :: !a
  , dwgeogUseSpheroid :: !a
  } deriving (Show, Eq, Functor, Foldable, Traversable)
$(deriveJSON (aesonDrop 6 snakeCase) ''DWithinGeogOp)

data OpExpG a
  = AEQ !Bool !a
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

  | ANISNULL -- IS NULL
  | ANISNOTNULL -- IS NOT NULL

  | CEQ !PGCol
  | CNE !PGCol
  | CGT !PGCol
  | CLT !PGCol
  | CGTE !PGCol
  | CLTE !PGCol
  deriving (Eq, Show, Functor, Foldable, Traversable)

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

  ANISNULL       -> ("_is_null", toJSON True)
  ANISNOTNULL    -> ("_is_null", toJSON False)

  CEQ a          -> ("_ceq", toJSON a)
  CNE a          -> ("_cne", toJSON a)
  CGT a          -> ("_cgt", toJSON a)
  CLT a          -> ("_clt", toJSON a)
  CGTE a         -> ("_cgte", toJSON a)
  CLTE a         -> ("_clte", toJSON a)

data AnnBoolExpFld a
  = AVCol !PGColInfo ![OpExpG a]
  | AVRel !RelInfo !(AnnBoolExp a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

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
  = PSESessVar !PgType !SessVar
  | PSESQLExp !S.SQLExp
  deriving (Show, Eq)

instance ToJSON PartialSQLExp where
  toJSON = \case
    PSESessVar colTy sessVar -> toJSON (colTy, sessVar)
    PSESQLExp e -> toJSON $ toSQLTxt e

instance ToJSON AnnBoolExpPartialSQL where
  toJSON = gBoolExpToJSON f
    where
      f annFld = case annFld of
        AVCol pci opExps ->
          ( getPGColTxt $ pgiName pci
          , toJSON (pci, map opExpSToJSON opExps)
          )
        AVRel ri relBoolExp ->
          ( relNameToTxt $ riName ri
          , toJSON (ri, toJSON relBoolExp)
          )
      opExpSToJSON :: OpExpG PartialSQLExp -> Value
      opExpSToJSON =
        object . pure . opExpToJPair toJSON
