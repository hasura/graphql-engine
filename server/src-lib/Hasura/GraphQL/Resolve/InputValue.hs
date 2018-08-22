{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Hasura.GraphQL.Resolve.InputValue
  ( withNotNull
  , tyMismatch
  , asPGColValM
  , asPGColVal
  , asEnumVal
  , withObject
  , withObjectM
  , withArray
  , withArrayM
  , parseMany
  ) where

import           Hasura.Prelude

import qualified Data.Text                     as T
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Utils
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

withNotNull
  :: (MonadError QErr m)
  => G.NamedType -> Maybe a -> m a
withNotNull nt v =
  onNothing v $ throw500 $
  "unexpected null for a value of type " <> showNamedTy nt

tyMismatch
  :: (MonadError QErr m) => Text -> AnnInpVal -> m a
tyMismatch expectedTy v =
  throw500 $ "expected " <> expectedTy <> ", found " <>
  getAnnInpValKind (_aivValue v) <> " for value of type " <>
  G.showGT (_aivType v)

asPGColValM
  :: (MonadError QErr m)
  => AnnInpVal -> m (Maybe (Maybe G.Variable, PGColType, PGColValue))
asPGColValM v = case _aivValue v of
  AGScalar colTy valM -> return $ fmap (_aivVariable v, colTy,) valM
  _                   -> tyMismatch "pgvalue" v

asPGColVal
  :: (MonadError QErr m)
  => AnnInpVal -> m (Maybe G.Variable, PGColType, PGColValue)
asPGColVal v = case _aivValue v of
  AGScalar colTy (Just val) -> return (_aivVariable v, colTy, val)
  AGScalar colTy Nothing ->
    throw500 $ "unexpected null for ty "
    <> T.pack (show colTy)
  _            -> tyMismatch "pgvalue" v

asEnumVal
  :: (MonadError QErr m)
  => AnnInpVal -> m (G.NamedType, G.EnumValue)
asEnumVal v = case _aivValue v of
  AGEnum ty (Just val) -> return (ty, val)
  AGEnum ty Nothing ->
    throw500 $ "unexpected null for ty " <> showNamedTy ty
  _              -> tyMismatch "enum" v

withObject
  :: (MonadError QErr m)
  => (G.NamedType -> AnnGObject -> m a) -> AnnInpVal -> m a
withObject fn v = case _aivValue v of
  AGObject nt (Just obj) -> fn nt obj
  AGObject _ Nothing  ->
    throw500 $ "unexpected null for ty" <> G.showGT (_aivType v)
  _               -> tyMismatch "object" v

withObjectM
  :: (MonadError QErr m)
  => (G.NamedType -> Maybe AnnGObject -> m a) -> AnnInpVal -> m a
withObjectM fn v = case _aivValue v of
  AGObject nt objM -> fn nt objM
  _                -> tyMismatch "object" v

withArrayM
  :: (MonadError QErr m)
  => (G.ListType -> Maybe [AnnInpVal] -> m a) -> AnnInpVal -> m a
withArrayM fn v = case _aivValue v of
  AGArray lt listM -> fn lt listM
  _                -> tyMismatch "array" v

withArray
  :: (MonadError QErr m)
  => (G.ListType -> [AnnInpVal] -> m a) -> AnnInpVal -> m a
withArray fn v = case _aivValue v of
  AGArray lt (Just l) -> fn lt l
  AGArray _ Nothing  -> throw500 $ "unexpected null for ty "
                         <> G.showGT (_aivType v)
  _                   -> tyMismatch "array" v

parseMany
  :: (MonadError QErr m)
  => (AnnInpVal -> m a) -> AnnInpVal -> m (Maybe [a])
parseMany fn v = case _aivValue v of
  AGArray _ arrM -> mapM (mapM fn) arrM
  _              -> tyMismatch "array" v
