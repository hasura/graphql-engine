module Hasura.GraphQL.Resolve.InputValue
  ( withNotNull
  , tyMismatch
  , asPGColumnTypeAndValueM
  , asPGColumnValueM
  , asPGColumnValue
  , asEnumVal
  , asEnumValM
  , withObject
  , asObject
  , withObjectM
  , asObjectM
  , withArray
  , asArray
  , withArrayM
  , parseMany
  , asPGColText
  , asPGColTextM
  ) where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax  as G

import qualified Hasura.RQL.Types               as RQL

import           Hasura.GraphQL.Resolve.Context
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

asPGColumnTypeAndValueM
  :: (MonadError QErr m)
  => AnnInpVal
  -> m (PGColumnType, WithScalarType (Maybe PGScalarValue))
asPGColumnTypeAndValueM v = case _aivValue v of
  AGScalar colTy val -> pure (PGColumnScalar colTy, WithScalarType colTy val)
  AGEnum _ (AGEReference reference maybeValue) -> do
    let maybeScalarValue = PGValText . RQL.getEnumValue <$> maybeValue
    pure (PGColumnEnumReference reference, WithScalarType PGText maybeScalarValue)
  _ -> tyMismatch "pgvalue" v

asPGColumnTypeAndAnnValueM :: (MonadError QErr m) => AnnInpVal -> m (PGColumnType, Maybe AnnPGVal)
asPGColumnTypeAndAnnValueM v = do
  (columnType, scalarValueM) <- asPGColumnTypeAndValueM v
  let mkAnnPGColVal = AnnPGVal (_aivVariable v) (G.isNullable (_aivType v)) columnType
  pure (columnType, mkAnnPGColVal <$> sequence scalarValueM)

asPGColumnValueM :: (MonadError QErr m) => AnnInpVal -> m (Maybe AnnPGVal)
asPGColumnValueM = fmap snd . asPGColumnTypeAndAnnValueM

asPGColumnValue :: (MonadError QErr m) => AnnInpVal -> m AnnPGVal
asPGColumnValue v = do
  (columnType, annPGValM) <- asPGColumnTypeAndAnnValueM v
  onNothing annPGValM $ throw500 ("unexpected null for type " <>> columnType)

-- | Note: only handles “synthetic” enums (see 'EnumValuesInfo'). Enum table references are handled
-- by 'asPGColumnTypeAndValueM' and its variants.
asEnumVal :: (MonadError QErr m) => AnnInpVal -> m (G.NamedType, G.EnumValue)
asEnumVal = asEnumValM >=> \case
  (ty, Just val) -> pure (ty, val)
  (ty, Nothing)  -> throw500 $ "unexpected null for ty " <> showNamedTy ty

-- | Like 'asEnumVal', only handles “synthetic” enums.
asEnumValM :: (MonadError QErr m) => AnnInpVal -> m (G.NamedType, Maybe G.EnumValue)
asEnumValM v = case _aivValue v of
  AGEnum ty (AGESynthetic valM) -> return (ty, valM)
  _                             -> tyMismatch "enum" v

withObject
  :: (MonadError QErr m)
  => (G.NamedType -> AnnGObject -> m a) -> AnnInpVal -> m a
withObject fn v = case _aivValue v of
  AGObject nt (Just obj) -> fn nt obj
  AGObject _ Nothing     ->
    throw500 $ "unexpected null for ty"
    <> G.showGT (_aivType v)
  _                      -> tyMismatch "object" v

asObject
  :: (MonadError QErr m)
  => AnnInpVal -> m AnnGObject
asObject = withObject (\_ o -> return o)

withObjectM
  :: (MonadError QErr m)
  => (G.NamedType -> Maybe AnnGObject -> m a) -> AnnInpVal -> m a
withObjectM fn v = case _aivValue v of
  AGObject nt objM -> fn nt objM
  _                -> tyMismatch "object" v

asObjectM
  :: (MonadError QErr m)
  => AnnInpVal -> m (Maybe AnnGObject)
asObjectM = withObjectM (\_ o -> return o)

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
  AGArray _ Nothing   -> throw500 $ "unexpected null for ty"
                         <> G.showGT (_aivType v)
  _                   -> tyMismatch "array" v

asArray
  :: (MonadError QErr m)
  => AnnInpVal -> m [AnnInpVal]
asArray = withArray (\_ vals -> return vals)

parseMany
  :: (MonadError QErr m)
  => (AnnInpVal -> m a) -> AnnInpVal -> m (Maybe [a])
parseMany fn v = case _aivValue v of
  AGArray _ arrM -> mapM (mapM fn) arrM
  _              -> tyMismatch "array" v

onlyText
  :: (MonadError QErr m)
  => PGScalarValue -> m Text
onlyText = \case
  PGValText t -> return t
  PGValVarchar t -> return t
  _           -> throw500 "expecting text for asPGColText"

asPGColText
  :: (MonadError QErr m)
  => AnnInpVal -> m Text
asPGColText val = do
  pgColVal <- pstValue . _apvValue <$> asPGColumnValue val
  onlyText pgColVal

asPGColTextM
  :: (MonadError QErr m)
  => AnnInpVal -> m (Maybe Text)
asPGColTextM val = do
  pgColValM <- fmap (pstValue . _apvValue) <$> asPGColumnValueM val
  mapM onlyText pgColValM
