{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.Types.BoolExp
       ( GBoolExp(..)
       , gBoolExpTrue
       , gBoolExpToJSON
       , parseGBoolExp

       , OpExpG(..)

       , AnnBoolExpFld(..)
       , AnnBoolExp
       -- , traverseAnnBoolExp
       , annBoolExpTrue
       , andAnnBoolExps

       , AnnBoolExpFldSQL
       , AnnBoolExpSQL
       , foldBoolExp
       ) where

import           Hasura.Prelude
import           Hasura.RQL.Types.Common
import qualified Hasura.SQL.DML             as S
import           Hasura.SQL.Types
-- import           Hasura.SQL.Value

import           Data.Aeson
-- import           Data.Aeson.TH
-- import           Data.Aeson.Casing
import           Data.Aeson.Internal
import qualified Data.Aeson.Types           as J
import qualified Data.HashMap.Strict        as M
-- import qualified Data.Text.Extended         as T
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)

data GBoolExp a
  = BoolAnd ![GBoolExp a]
  | BoolOr  ![GBoolExp a]
  | BoolNot !(GBoolExp a)
  | BoolFld !a
  deriving (Show, Eq, Lift, Functor, Foldable, Traversable)

gBoolExpTrue :: GBoolExp a
gBoolExpTrue = BoolAnd []

gBoolExpToJSON :: (a -> (Text, Value)) -> GBoolExp a -> Value
gBoolExpToJSON f = \case
  BoolAnd bExps -> object ["$and" .= map (gBoolExpToJSON f) bExps ]
  BoolOr bExps  -> object ["$or" .= map (gBoolExpToJSON f) bExps ]
  BoolNot bExp  -> object ["$not" .= gBoolExpToJSON f bExp ]
  BoolFld a     -> object $ pure $ f a

-- instance ToJSON (GBoolExp ColExp) where
--   toJSON (BoolAnd bExps) =
--     object $ flip map bExps $ \case
--     BoolOr cbExps        -> "$or" .= cbExps
--     BoolAnd cbExps       -> "$and" .= cbExps
--     BoolFld (ColExp k v) -> getFieldNameTxt k .= v
--     BoolNot notExp       -> "$not" .= notExp
--   toJSON (BoolOr bExps) =
--     object $ flip map bExps $ \case
--     BoolOr cbExps        -> "$or" .= cbExps
--     BoolAnd cbExps       -> "$and" .= cbExps
--     BoolFld (ColExp k v) -> getFieldNameTxt k .= v
--     BoolNot notExp       -> "$not" .= notExp
--   toJSON (BoolFld (ColExp k v)) =
--     object [ getFieldNameTxt k .= v ]
--   toJSON (BoolNot notExp) =
--     object [ "$not" .= notExp ]

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

-- instance FromJSON (GBoolExp ColExp) where
--   parseJSON (Object o) = do
--     boolExps <- forM (M.toList o) $ \(k, v) -> if
--       | k == "$or"  -> BoolOr  <$> parseJSON v <?> Key k
--       | k == "_or"  -> BoolOr  <$> parseJSON v <?> Key k
--       | k == "$and" -> BoolAnd <$> parseJSON v <?> Key k
--       | k == "_and" -> BoolAnd <$> parseJSON v <?> Key k
--       | k == "$not" -> BoolNot <$> parseJSON v <?> Key k
--       | k == "_not" -> BoolNot <$> parseJSON v <?> Key k
--       | otherwise   -> BoolFld . ColExp (FieldName k) <$> parseJSON v
--     return $ BoolAnd boolExps
--   parseJSON _ = fail "expecting an Object for boolean exp"

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

data OpExpG a
  = AEQ !a
  | ANE !a

  | AIN  ![a]
  | ANIN ![a]

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
  | AHasKeysAny [Text]
  | AHasKeysAll [Text]

  | ANISNULL -- IS NULL
  | ANISNOTNULL -- IS NOT NULL

  | CEQ !PGCol
  | CNE !PGCol
  | CGT !PGCol
  | CLT !PGCol
  | CGTE !PGCol
  | CLTE !PGCol
  deriving (Eq, Show, Functor, Foldable, Traversable)


opExpToJPair :: (a -> Value) -> OpExpG a -> (Text, Value)
opExpToJPair f = \case
  AEQ a          -> ("_eq", f a)
  ANE a          -> ("_ne", f a)

  AIN a          -> ("_in", toJSON $ map f a)
  ANIN a         -> ("_nin", toJSON $ map f a)

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
  AHasKeysAny a  -> ("_has_keys_any", toJSON a)
  AHasKeysAll a  -> ("_has_keys_all", toJSON a)

  ANISNULL       -> ("_is_null", toJSON True)
  ANISNOTNULL    -> ("_is_null", toJSON False)

  CEQ a          -> ("_ceq", toJSON a)
  CNE a          -> ("_cne", toJSON a)
  CGT a          -> ("_cgt", toJSON a)
  CLT a          -> ("_clt", toJSON a)
  CGTE a         -> ("_cgte", toJSON a)
  CLTE a         -> ("_clte", toJSON a)

-- data OpExpG a
--   = OEVal !(AnnValOpExpG a)
--   | OECol !ColOp !PGCol
--   deriving (Show, Eq)

-- type OpExp = OpExpG (PGColType, PGColValue)

-- data AnnBoolExpFldG a b
--   = AVCol !PGColInfo !a
--   | AVRel !RelInfo !b
--   deriving (Show, Eq)

-- instance Bifunctor AnnBoolExpFldG where
--   bimap f g = \case
--     AVCol ci a -> AVCol ci $ f a
--     AVRel ri b -> AVRel ri $ g b

-- newtype AnnBoolExpFld a
--   = AnnBoolExpFld { unAnnBoolExpFld :: AnnBoolExpFldG [OpExpG a] (AnnBoolExp a) }
--   deriving (Show, Eq)

-- instance Functor AnnBoolExpFld where
--   fmap f (AnnBoolExpFld annBoolExpFld) =
--     AnnBoolExpFld $ bimap (map (fmap f)) (fmap f) annBoolExpFld

data AnnBoolExpFld a
  = AVCol !PGColInfo ![OpExpG a]
  | AVRel !RelInfo !(AnnBoolExp a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

type AnnBoolExp a
  = GBoolExp (AnnBoolExpFld a)

annBoolExpTrue :: AnnBoolExp a
annBoolExpTrue = gBoolExpTrue

andAnnBoolExps :: AnnBoolExp a -> AnnBoolExp a -> AnnBoolExp a
andAnnBoolExps l r =
  BoolAnd [l, r]

-- traverseAnnBoolExp
--   :: (Applicative f)
--   => (AnnBoolExpFld a -> f (AnnBoolExpFld b))
--   -> AnnBoolExp a
--   -> f (AnnBoolExp b)
-- traverseAnnBoolExp f boolExp =
--   traverse f boolExp

type AnnBoolExpFldSQL = AnnBoolExpFld S.SQLExp
type AnnBoolExpSQL = AnnBoolExp S.SQLExp

instance ToJSON AnnBoolExpSQL where
  toJSON = gBoolExpToJSON f
    where
      f annFld = case annFld of
        AVCol pci opExps ->
          ( getPGColTxt $ pgiName pci
          , toJSON (pci, map opExpSToJSON opExps)
          )
        AVRel ri relBoolExp ->
          ( getRelTxt $ riName ri
          , toJSON (ri, toJSON relBoolExp)
          )
      opExpSToJSON :: OpExpG S.SQLExp -> Value
      opExpSToJSON =
        object . pure . opExpToJPair (toJSON . toSQLTxt)

-- $(deriveToJSON
--   defaultOptions{constructorTagModifier = snakeCase . drop 2}
--   ''AnnBoolExpFldG)

-- type AnnValO a = AnnBoolExpFldG [OpExpG a]
-- type AnnVal = AnnValO (PGColType, PGColValue)

-- data ColOp
--   = CEQ
--   | CNE
--   | CGT
--   | CLT
--   | CGTE
--   | CLTE
--   deriving (Eq)

-- instance Show ColOp where
--   show CEQ  = "$ceq"
--   show CNE  = "$cne"

--   show CGT  = "$cgt"
--   show CLT  = "$clt"
--   show CGTE = "$cgte"
--   show CLTE = "$clte"

-- data RQLOp
--   = REQ -- equals
--   | RNE -- <>

--   | RIN  -- in an array
--   | RNIN -- not in an array

--   | RGT    -- >
--   | RLT    -- <
--   | RGTE   -- >=
--   | RLTE   -- <=

--   | RLIKE  -- LIKE
--   | RNLIKE  -- NOT LIKE

--   | RILIKE  -- ILIKE, case insensitive
--   | RNILIKE -- NOT ILIKE, case insensitive

--   | RSIMILAR  -- similar, regex
--   | RNSIMILAR -- not similar, regex

--   | RISNULL -- is null

--   deriving (Eq)

-- instance Show RQLOp where
--   show REQ       = "$eq"
--   show RNE       = "$ne"

--   show RIN       = "$in"
--   show RNIN      = "$nin"

--   show RGT       = "$gt"
--   show RLT       = "$lt"
--   show RGTE      = "$gte"
--   show RLTE      = "$lte"

--   show RLIKE     = "$like"
--   show RNLIKE    = "$nlike"

--   show RILIKE    = "$ilike"
--   show RNILIKE   = "$nilike"

--   show RSIMILAR  = "$similar"
--   show RNSIMILAR = "$nsimilar"

--   show RISNULL   = "$is_null"

-- instance DQuote RQLOp where
--   dquoteTxt op = T.pack $ show op
