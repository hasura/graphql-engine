{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}

module Hasura.GraphQL.Context where

import           Data.Aeson
import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict                 as Map
import qualified Data.Text                           as T
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.GraphQL.Resolve.ContextTypes
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types.Permission
import           Hasura.SQL.Types

import qualified Hasura.SQL.DML                      as S

type OpCtxMap = Map.HashMap G.Name OpCtx

data OpCtx
  -- table, req hdrs
  = OCInsert QualifiedTable [T.Text]
  -- tn, filter exp, limit, req hdrs
  | OCSelect QualifiedTable S.BoolExp (Maybe Int) [T.Text]
  -- tn, filter exp, reqt hdrs
  | OCSelectPkey QualifiedTable S.BoolExp [T.Text]
  -- tn, filter exp, limit, req hdrs
  | OCSelectAgg QualifiedTable S.BoolExp (Maybe Int) [T.Text]
  -- tn, filter exp, req hdrs
  | OCUpdate QualifiedTable S.BoolExp [T.Text]
  -- tn, filter exp, req hdrs
  | OCDelete QualifiedTable S.BoolExp [T.Text]
  deriving (Show, Eq)

data GCtx
  = GCtx
  { _gTypes     :: !TypeMap
  , _gFields    :: !FieldMap
  , _gOrdByCtx  :: !OrdByCtx
  , _gQueryRoot :: !ObjTyInfo
  , _gMutRoot   :: !(Maybe ObjTyInfo)
  , _gSubRoot   :: !(Maybe ObjTyInfo)
  , _gOpCtxMap  :: !OpCtxMap
  , _gInsCtxMap :: !InsCtxMap
  } deriving (Show, Eq)

instance Has TypeMap GCtx where
  getter = _gTypes
  modifier f ctx = ctx { _gTypes = f $ _gTypes ctx }

instance ToJSON GCtx where
  toJSON _ = String "GCtx"

type GCtxMap = Map.HashMap RoleName GCtx

data TyAgg
  = TyAgg
  { _taTypes  :: !TypeMap
  , _taFields :: !FieldMap
  , _taOrdBy  :: !OrdByCtx
  } deriving (Show, Eq)

instance Semigroup TyAgg where
  (TyAgg t1 f1 o1) <> (TyAgg t2 f2 o2) =
    TyAgg (Map.union t1 t2) (Map.union f1 f2) (Map.union o1 o2)

instance Monoid TyAgg where
  mempty = TyAgg Map.empty Map.empty Map.empty
  mappend = (<>)

newtype RootFlds
  = RootFlds
  { _taMutation :: Map.HashMap G.Name (OpCtx, Either ObjFldInfo ObjFldInfo)
  } deriving (Show, Eq)

instance Semigroup RootFlds where
  (RootFlds m1) <> (RootFlds m2)
    = RootFlds (Map.union m1 m2)

instance Monoid RootFlds where
  mempty = RootFlds Map.empty
  mappend  = (<>)
