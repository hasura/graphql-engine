{-# LANGUAGE DeriveLift                 #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Hasura.RQL.Types.Common
       ( RelName(..)
       , RelType(..)
       , relTypeToTxt

       , FieldName(..)
       , fromPGCol
       , fromRel

       , ColExp(..)
       , GBoolExp(..)
       , BoolExp
       , foldBoolExp

       , TQueryName(..)
       , TemplateParam(..)

       , ToAesonPairs(..)
       , WithTable(..)
       ) where

import           Hasura.Prelude
import qualified Hasura.SQL.DML             as S
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Internal
import qualified Data.HashMap.Strict        as M
import qualified Data.Text                  as T
import qualified Database.PG.Query          as Q
import           Instances.TH.Lift          ()
import           Language.Haskell.TH.Syntax (Lift)
import qualified PostgreSQL.Binary.Decoding as PD

newtype RelName
  = RelName {getRelTxt :: T.Text}
  deriving (Show, Eq, Hashable, FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Lift)

instance IsIden RelName where
  toIden (RelName r) = Iden r

instance DQuote RelName where
  dquoteTxt (RelName r) = r

relTypeToTxt :: RelType -> T.Text
relTypeToTxt ObjRel = "object"
relTypeToTxt ArrRel = "array"

data RelType
  = ObjRel
  | ArrRel
  deriving (Show, Eq)

instance ToJSON RelType where
  toJSON = String . relTypeToTxt

instance FromJSON RelType where
  parseJSON (String "object") = return ObjRel
  parseJSON (String "array") = return ArrRel
  parseJSON _ = fail "expecting either 'object' or 'array' for rel_type"

instance Q.FromCol RelType where
  fromCol bs = flip Q.fromColHelper bs $ PD.enum $ \case
    "object" -> Just ObjRel
    "array"  -> Just ArrRel
    _   -> Nothing

newtype FieldName
  = FieldName { getFieldNameTxt :: T.Text }
  deriving (Show, Eq, Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey, Lift)

instance IsIden FieldName where
  toIden (FieldName f) = Iden f

instance DQuote FieldName where
  dquoteTxt (FieldName c) = c

fromPGCol :: PGCol -> FieldName
fromPGCol (PGCol c) = FieldName c

fromRel :: RelName -> FieldName
fromRel (RelName r) = FieldName r

type BoolExp = GBoolExp ColExp

data ColExp
  = ColExp
  { ceCol :: !FieldName
  , ceVal :: !Value
  } deriving (Show, Eq, Lift)

data GBoolExp a
  = BoolAnd ![GBoolExp a]
  | BoolOr  ![GBoolExp a]
  | BoolCol !a
  | BoolNot !(GBoolExp a)
  deriving (Show, Eq, Lift, Functor, Foldable, Traversable)

instance ToJSON (GBoolExp ColExp) where
  toJSON (BoolAnd bExps) =
    object $ flip map bExps $ \bExp -> case bExp of
    BoolOr cbExps        -> "$or" .= cbExps
    BoolAnd cbExps       -> "$and" .= cbExps
    BoolCol (ColExp k v) -> getFieldNameTxt k .= v
    BoolNot notExp       -> "$not" .= notExp
  toJSON (BoolOr bExps) =
    object $ flip map bExps $ \bExp -> case bExp of
    BoolOr cbExps        -> "$or" .= cbExps
    BoolAnd cbExps       -> "$and" .= cbExps
    BoolCol (ColExp k v) -> getFieldNameTxt k .= v
    BoolNot notExp       -> "$not" .= notExp
  toJSON (BoolCol (ColExp k v)) =
    object [ getFieldNameTxt k .= v ]
  toJSON (BoolNot notExp) =
    object [ "$not" .= notExp ]

instance FromJSON (GBoolExp ColExp) where
  parseJSON (Object o) = do
    boolExps <- forM (M.toList o) $ \(k, v) -> if
      | k == "$or"  -> BoolOr  <$> parseJSON v <?> Key k
      | k == "_or"  -> BoolOr  <$> parseJSON v <?> Key k
      | k == "$and" -> BoolAnd <$> parseJSON v <?> Key k
      | k == "_and" -> BoolAnd <$> parseJSON v <?> Key k
      | k == "$not" -> BoolNot <$> parseJSON v <?> Key k
      | k == "_not" -> BoolNot <$> parseJSON v <?> Key k
      | otherwise   -> fmap (BoolCol . ColExp (FieldName k)) $ parseJSON v
    return $ BoolAnd boolExps
  parseJSON _ = fail "expecting an Object for boolean exp"

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
foldBoolExp f (BoolCol ce)  =
  f ce

newtype TQueryName
  = TQueryName { getTQueryName :: T.Text }
  deriving ( Show, Eq, Hashable, FromJSONKey, ToJSONKey
           , FromJSON, ToJSON, Q.ToPrepArg, Q.FromCol, Lift)

instance IsIden TQueryName where
  toIden (TQueryName r) = Iden r

instance DQuote TQueryName where
  dquoteTxt (TQueryName r) = r

newtype TemplateParam
  = TemplateParam { getTemplateParam :: T.Text }
  deriving (Show, Eq, Hashable, FromJSON, FromJSONKey, ToJSONKey, ToJSON, Lift)

instance DQuote TemplateParam where
  dquoteTxt (TemplateParam r) = r

class ToAesonPairs a where
  toAesonPairs :: (KeyValue v) => a -> [v]

data WithTable a
  = WithTable
  { wtName :: !QualifiedTable
  , wtInfo :: !a
  } deriving (Show, Eq, Lift)

instance (FromJSON a) => FromJSON (WithTable a) where
  parseJSON v@(Object o) =
    WithTable <$> o .: "table" <*> parseJSON v
  parseJSON _ =
    fail "expecting an Object with key 'table'"

instance (ToAesonPairs a) => ToJSON (WithTable a) where
  toJSON (WithTable tn rel) =
    object $ ("table" .= tn):toAesonPairs rel
