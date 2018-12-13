module Hasura.GraphQL.Resolve.InputValue
  ( withNotNull
  , tyMismatch
  , asPGColValM
  , asPGColVal
  , asEnumVal
  , withObject
  , asObject
  , withObjectM
  , withArray
  , asArray
  , withArrayM
  , parseMany
  , asPGColText
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
  :: (MonadError QErr m) => Text -> AnnGValue -> m a
tyMismatch expectedTy v =
  throw500 $ "expected " <> expectedTy <> ", found " <>
  getAnnInpValKind v <> " for value of type " <>
  G.showGT (getAnnInpValTy v)

asPGColValM
  :: (MonadError QErr m)
  => AnnGValue -> m (Maybe (PGColType, PGColValue))
asPGColValM = \case
  AGScalar colTy valM -> return $ fmap (colTy,) valM
  v            -> tyMismatch "pgvalue" v

asPGColVal
  :: (MonadError QErr m)
  => AnnGValue -> m (PGColType, PGColValue)
asPGColVal = \case
  AGScalar colTy (Just val) -> return (colTy, val)
  AGScalar colTy Nothing ->
    throw500 $ "unexpected null for ty "
    <> T.pack (show colTy)
  v            -> tyMismatch "pgvalue" v

asEnumVal
  :: (MonadError QErr m)
  => AnnGValue -> m (G.NamedType, G.EnumValue)
asEnumVal = \case
  AGEnum ty (Just val) -> return (ty, val)
  AGEnum ty Nothing ->
    throw500 $ "unexpected null for ty " <> showNamedTy ty
  v              -> tyMismatch "enum" v

withObject
  :: (MonadError QErr m)
  => (G.NamedType -> AnnGObject -> m a) -> AnnGValue -> m a
withObject fn v = case v of
  AGObject nt (Just obj) -> fn nt obj
  AGObject nt Nothing  ->
    throw500 $ "unexpected null for ty"
    <> G.showGT (G.TypeNamed (G.Nullability True) nt)
  _               -> tyMismatch "object" v

asObject
  :: (MonadError QErr m)
  => AnnGValue -> m AnnGObject
asObject = withObject (\_ o -> return o)

withObjectM
  :: (MonadError QErr m)
  => (G.NamedType -> Maybe AnnGObject -> m a) -> AnnGValue -> m a
withObjectM fn v = case v of
  AGObject nt objM -> fn nt objM
  _                -> tyMismatch "object" v

withArrayM
  :: (MonadError QErr m)
  => (G.ListType -> Maybe [AnnGValue] -> m a) -> AnnGValue -> m a
withArrayM fn v = case v of
  AGArray lt listM -> fn lt listM
  _                -> tyMismatch "array" v

withArray
  :: (MonadError QErr m)
  => (G.ListType -> [AnnGValue] -> m a) -> AnnGValue -> m a
withArray fn v = case v of
  AGArray lt (Just l) -> fn lt l
  AGArray lt Nothing  -> throw500 $ "unexpected null for ty"
                         <> G.showGT (G.TypeList (G.Nullability True) lt)
  _                   -> tyMismatch "array" v

asArray
  :: (MonadError QErr m)
  => AnnGValue -> m [AnnGValue]
asArray = withArray (\_ vals -> return vals)

parseMany
  :: (MonadError QErr m)
  => (AnnGValue -> m a) -> AnnGValue -> m (Maybe [a])
parseMany fn v = case v of
  AGArray _ arrM -> mapM (mapM fn) arrM
  _              -> tyMismatch "array" v

asPGColText
  :: (MonadError QErr m)
  => AnnGValue -> m Text
asPGColText val = do
  (_, pgColVal) <- asPGColVal val
  case pgColVal of
    PGValText t -> return t
    _           -> throw500 "expecting text for asPGColText"
