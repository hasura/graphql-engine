module Hasura.GraphQL.Resolve.InputValue
  ( withNotNull
  , tyMismatch

  , OpaqueValue
  , OpaquePGValue
  , mkParameterizablePGValue
  , openOpaqueValue
  , asPGColumnTypeAndValueM
  , asPGColumnValueM
  , asPGColumnValue

  , asScalarValM
  , asScalarVal
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
  , annInpValueToJson
  ) where

import           Hasura.Prelude

import qualified Text.Builder                   as TB

import qualified Language.GraphQL.Draft.Syntax  as G

import qualified Data.Aeson                     as J
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

-- | As part of query reusability tracking (see 'QueryReusability'), functions that parse input
-- values call 'markNotReusable' when the value comes from a variable. However, always calling
-- 'markNotReusable' when parsing column values (using 'asPGColumnValue' and its variants) would be
-- much too conservative: often the value is simply validated and wrapped immediately in 'UVPG',
-- which allows it to be parameterized over.
--
-- Always omitting the check would be incorrect, as some callers inspect the column values and use
-- them to generate different SQL, which is where 'OpaqueValue' comes in. Functions like
-- 'asPGColumnValue' return an 'OpaquePGValue', which can be safely converted to an 'UnresolvedVal'
-- via 'mkParameterizablePGValue' without marking the query as non-reusable. Other callers that wish
-- to inspect the value can instead call 'openOpaqueValue' to get the value out, and /that/ will
-- mark the query non-reusable, instead.
--
-- In other words, 'OpaqueValue' is a mechanism of delaying the 'markNotReusable' call until we’re
-- confident its value will actually affect the generated SQL.
data OpaqueValue a
  = OpaqueValue
  { _opgvValue      :: !a
  , _opgvIsVariable :: !Bool
  }  deriving (Show)
type OpaquePGValue = OpaqueValue AnnPGVal

mkParameterizablePGValue :: OpaquePGValue -> UnresolvedVal
mkParameterizablePGValue (OpaqueValue v _) = UVPG v

openOpaqueValue :: (MonadReusability m) => OpaqueValue a -> m a
openOpaqueValue (OpaqueValue v isVariable) = when isVariable markNotReusable $> v

asPGColumnTypeAndValueM
  :: (MonadReusability m, MonadError QErr m)
  => AnnInpVal
  -> m (PGColumnType, WithScalarType (Maybe (OpaqueValue PGScalarValue)))
asPGColumnTypeAndValueM v = do
  (columnType, scalarValueM) <- case _aivValue v of
    AGScalar colTy val -> pure (PGColumnScalar colTy, WithScalarType colTy val)
    AGEnum _ (AGEReference reference maybeValue) -> do
      let maybeScalarValue = PGValText . RQL.getEnumValue <$> maybeValue
      pure (PGColumnEnumReference reference, WithScalarType PGText maybeScalarValue)
    _ -> tyMismatch "pgvalue" v

  for_ (_aivVariable v) $ \variableName -> if
    -- If the value is a nullable variable, then the caller might make a different decision based on
    -- whether the result is 'Nothing' or 'Just', which would change the generated query, so we have
    -- to unconditionally mark the query non-reusable.
    | G.isNullable (_aivType v) -> markNotReusable
    | otherwise                 -> recordVariableUse variableName columnType

  let isVariable = isJust $ _aivVariable v
  pure (columnType, fmap (flip OpaqueValue isVariable) <$> scalarValueM)

asPGColumnTypeAndAnnValueM
  :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m (PGColumnType, Maybe OpaquePGValue)
asPGColumnTypeAndAnnValueM v = do
  (columnType, scalarValueM) <- asPGColumnTypeAndValueM v
  let mkAnnPGColVal = AnnPGVal (_aivVariable v) (G.isNullable (_aivType v))
      replaceOpaqueValue (WithScalarType scalarType (OpaqueValue scalarValue isVariable)) =
        OpaqueValue (mkAnnPGColVal (WithScalarType scalarType scalarValue)) isVariable
  pure (columnType, replaceOpaqueValue <$> sequence scalarValueM)

asPGColumnValueM :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m (Maybe OpaquePGValue)
asPGColumnValueM = fmap snd . asPGColumnTypeAndAnnValueM

asPGColumnValue :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m OpaquePGValue
asPGColumnValue v = do
  (columnType, annPGValM) <- asPGColumnTypeAndAnnValueM v
  onNothing annPGValM $ throw500 ("unexpected null for type " <>> columnType)

openInputValue :: (MonadReusability m) => AnnInpVal -> m AnnGValue
openInputValue v = when (isJust $ _aivVariable v) markNotReusable $> _aivValue v

asScalarValM :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> PGScalarType -> m (Maybe PGScalarValue)
asScalarValM v tp = openInputValue v >>= \case
  AGScalar tp' vM ->
    if tp == tp'
    then pure vM
    else tyMismatch "scalar" v
  _ -> tyMismatch "scalar" v

asScalarVal :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> PGScalarType -> m PGScalarValue
asScalarVal v tp = asScalarValM v tp >>= \case
  Just val -> pure val
  Nothing -> throw500 $ "unexpected null for ty " <> TB.run (toSQL tp)

-- | Note: only handles “synthetic” enums (see 'EnumValuesInfo'). Enum table references are handled
-- by 'asPGColumnType' and its variants.
asEnumVal :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m (G.NamedType, G.EnumValue)
asEnumVal = asEnumValM >=> \case
  (ty, Just val) -> pure (ty, val)
  (ty, Nothing)  -> throw500 $ "unexpected null for ty " <> showNamedTy ty

-- | Like 'asEnumVal', only handles “synthetic” enums.
asEnumValM :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m (G.NamedType, Maybe G.EnumValue)
asEnumValM v = openInputValue v >>= \case
  AGEnum ty (AGESynthetic valM) -> return (ty, valM)
  _                             -> tyMismatch "enum" v

withObject
  :: (MonadReusability m, MonadError QErr m) => (G.NamedType -> AnnGObject -> m a) -> AnnInpVal -> m a
withObject fn v = openInputValue v >>= \case
  AGObject nt (Just obj) -> fn nt obj
  AGObject _ Nothing     ->
    throw500 $ "unexpected null for ty"
    <> G.showGT (_aivType v)
  _                      -> tyMismatch "object" v

asObject :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m AnnGObject
asObject = withObject (\_ o -> return o)

withObjectM
  :: (MonadReusability m, MonadError QErr m)
  => (G.NamedType -> Maybe AnnGObject -> m a) -> AnnInpVal -> m a
withObjectM fn v = openInputValue v >>= \case
  AGObject nt objM -> fn nt objM
  _                -> tyMismatch "object" v

asObjectM :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m (Maybe AnnGObject)
asObjectM = withObjectM (\_ o -> return o)

withArrayM
  :: (MonadReusability m, MonadError QErr m)
  => (G.ListType -> Maybe [AnnInpVal] -> m a) -> AnnInpVal -> m a
withArrayM fn v = openInputValue v >>= \case
  AGArray lt listM -> fn lt listM
  _                -> tyMismatch "array" v

withArray
  :: (MonadReusability m, MonadError QErr m)
  => (G.ListType -> [AnnInpVal] -> m a) -> AnnInpVal -> m a
withArray fn v = openInputValue v >>= \case
  AGArray lt (Just l) -> fn lt l
  AGArray _ Nothing   -> throw500 $ "unexpected null for ty"
                         <> G.showGT (_aivType v)
  _                   -> tyMismatch "array" v

asArray :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m [AnnInpVal]
asArray = withArray (\_ vals -> return vals)

parseMany
  :: (MonadReusability m, MonadError QErr m) => (AnnInpVal -> m a) -> AnnInpVal -> m (Maybe [a])
parseMany fn v = openInputValue v >>= \case
  AGArray _ arrM -> mapM (mapM fn) arrM
  _              -> tyMismatch "array" v

onlyText
  :: (MonadError QErr m)
  => PGScalarValue -> m Text
onlyText = \case
  PGValText t    -> return t
  PGValVarchar t -> return t
  _           -> throw500 "expecting text for asPGColText"

asPGColText :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m Text
asPGColText val = do
  pgColVal <- openOpaqueValue =<< asPGColumnValue val
  onlyText (pstValue $ _apvValue pgColVal)

asPGColTextM :: (MonadReusability m, MonadError QErr m) => AnnInpVal -> m (Maybe Text)
asPGColTextM val = do
  pgColValM <- traverse openOpaqueValue =<< asPGColumnValueM val
  traverse onlyText (pstValue . _apvValue <$> pgColValM)

annInpValueToJson :: AnnInpVal -> J.Value
annInpValueToJson annInpValue =
  case _aivValue annInpValue of
    AGScalar _ pgColumnValueM -> maybe J.Null pgScalarValueToJson pgColumnValueM
    AGEnum _ enumValue        -> case enumValue of
      AGESynthetic enumValueM   -> J.toJSON enumValueM
      AGEReference _ enumValueM -> J.toJSON enumValueM
    AGObject _ objectM        -> J.toJSON $ fmap (fmap annInpValueToJson) objectM
    AGArray _ valuesM         -> J.toJSON $ fmap (fmap annInpValueToJson) valuesM
