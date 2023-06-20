{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}

module Hasura.Base.Error
  ( Code (..),
    QErr (..),
    QErrExtra (..),
    overrideQErrStatus,
    prefixQErr,
    showQErr,
    encodeQErr,
    encodeGQLErr,
    err400,
    err404,
    err405,
    err401,
    err409,
    err429,
    err500,
    internalError,
    QErrM,
    throw400,
    throw400WithDetail,
    throw404,
    throw405,
    throw409,
    throw429,
    throw500,
    throw500WithDetail,
    throw401,
    iResultToMaybe,
    -- Aeson helpers
    runAesonParser,
    decodeValue,
    -- Modify error messages
    modifyErr,
    modifyErrAndSet500,
    modifyQErr,
    modifyErrA,
    -- Attach context
    withPathK,
    withPathKA,
    withPathI,
    withPathIA,
    indexedFoldlA',
    indexedForM,
    indexedMapM,
    indexedForM_,
    indexedMapM_,
    indexedTraverseA_,
    -- Lens related
    qeInternalLens,
    _ExtraExtensions,
    _ExtraInternal,
  )
where

import Control.Arrow.Extended
import Control.Lens (makeLensesFor, makePrisms)
import Data.Aeson
import Data.Aeson.Encoding qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.Types
import Data.Parser.JSONPath (encodeJSONPath)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Database.PG.Query qualified as PG
import Hasura.Prelude
import Network.HTTP.Types qualified as HTTP

data Code
  = AccessDenied
  | ActionWebhookCode !Text
  | AlreadyExists
  | AlreadyTracked
  | AlreadyUntracked
  | BadRequest
  | BigQueryError
  | Busy
  | ConcurrentUpdate
  | ConnectionNotEstablished
  | CoercionError
  | Conflict
  | ConstraintError
  | ConstraintViolation
  | -- | Custom code for extending this sum-type easily
    CustomCode !Text
  | CyclicDependency
  | DataException
  | DataConnectorError
  | DependencyError
  | InvalidConfiguration
  | InvalidHeaders
  | InvalidJSON
  | InvalidParams
  | JWTInvalid
  | JWTInvalidClaims
  | JWTRoleClaimMissing
  | MSSQLError
  | MethodNotAllowed
  | NotExists
  | NotFound
  | NotSupported
  | ParseFailed
  | PermissionDenied
  | PermissionError
  | PostgresError
  | PostgresMaxConnectionsError
  | RemoteSchemaConflicts
  | RemoteSchemaError
  | -- | Websockets
    StartFailed
  | Unexpected
  | UnexpectedPayload
  | ValidationFailed
  | -- | Connection templates
    TemplateResolutionFailed
  | TimeoutErrorCode
  deriving (Show, Eq)

instance ToJSON Code where
  toJSON code = String $ case code of
    AccessDenied -> "access-denied"
    ActionWebhookCode t -> t
    AlreadyExists -> "already-exists"
    AlreadyTracked -> "already-tracked"
    AlreadyUntracked -> "already-untracked"
    BadRequest -> "bad-request"
    BigQueryError -> "bigquery-error"
    Busy -> "busy"
    ConcurrentUpdate -> "concurrent-update"
    ConnectionNotEstablished -> "connection-not-established"
    CoercionError -> "coercion-error"
    Conflict -> "conflict"
    ConstraintError -> "constraint-error"
    ConstraintViolation -> "constraint-violation"
    CustomCode t -> t
    CyclicDependency -> "cyclic-dependency"
    DataException -> "data-exception"
    DataConnectorError -> "data-connector-error"
    DependencyError -> "dependency-error"
    InvalidConfiguration -> "invalid-configuration"
    InvalidHeaders -> "invalid-headers"
    InvalidJSON -> "invalid-json"
    InvalidParams -> "invalid-params"
    JWTInvalid -> "invalid-jwt"
    JWTInvalidClaims -> "jwt-invalid-claims"
    JWTRoleClaimMissing -> "jwt-missing-role-claims"
    MSSQLError -> "mssql-error"
    MethodNotAllowed -> "method-not-allowed"
    NotExists -> "not-exists"
    NotFound -> "not-found"
    NotSupported -> "not-supported"
    ParseFailed -> "parse-failed"
    PermissionDenied -> "permission-denied"
    PermissionError -> "permission-error"
    PostgresError -> "postgres-error"
    PostgresMaxConnectionsError -> "postgres-max-connections-error"
    RemoteSchemaConflicts -> "remote-schema-conflicts"
    RemoteSchemaError -> "remote-schema-error"
    StartFailed -> "start-failed"
    Unexpected -> "unexpected"
    UnexpectedPayload -> "unexpected-payload"
    ValidationFailed -> "validation-failed"
    TemplateResolutionFailed -> "template-resolution-failed"
    TimeoutErrorCode -> "timeout"

data QErr = QErr
  { qePath :: JSONPath,
    qeStatus :: HTTP.Status,
    qeError :: Text,
    qeCode :: Code,
    qeInternal :: Maybe QErrExtra
  }
  deriving (Eq)

-- | Extra context for a QErr, which can either be information from an internal
-- error (e.g. from Postgres, or from a network operation timing out), or
-- context provided when an external service or operation fails, for instance, a
-- webhook error response may provide additional context in the `extensions`
-- key.
data QErrExtra
  = ExtraExtensions Value
  | ExtraInternal Value
  deriving (Eq)

instance ToJSON QErrExtra where
  toJSON = \case
    ExtraExtensions v -> v
    ExtraInternal v -> v

instance ToJSON QErr where
  toJSON (QErr jPath _ msg code extra) =
    object $ case extra of
      Just (ExtraInternal e) -> "internal" .= e : err
      Just ExtraExtensions {} -> err
      Nothing -> err
    where
      err =
        [ "error" .= msg,
          "path" .= encodeJSONPath jPath,
          "code" .= code
        ]
  toEncoding (QErr jPath _ msg code extra) =
    pairs $ case extra of
      Just (ExtraInternal e) -> err <> "internal" .= e -- Internal comes after all other properties so is the last thing the user sees
      Just ExtraExtensions {} -> err
      Nothing -> err
    where
      err =
        -- error property comes first so the error message is the first
        -- thing the user sees
        ("error" .= msg)
          <> ("path" .= encodeJSONPath jPath)
          <> ("code" .= code)

-- | Overrides the status and code of a QErr while retaining all other fields.
overrideQErrStatus :: HTTP.Status -> Code -> QErr -> QErr
overrideQErrStatus newStatus newCode err = err {qeStatus = newStatus, qeCode = newCode}

-- | Prefixes the message of a QErr while retaining all other fields.
prefixQErr :: Text -> QErr -> QErr
prefixQErr prefix err = err {qeError = prefix <> qeError err}

-- Temporary function until we have a better one in place.
showQErr :: QErr -> Text
showQErr = TL.toStrict . TL.decodeUtf8 . encode

encodeGQLErr :: Bool -> QErr -> Encoding
encodeGQLErr includeInternal (QErr jPath _ msg code maybeExtra) =
  pairs (("message" .= msg) <> (J.pair "extensions" extnsObj))
  where
    appendIf cond a b = if cond then a <> b else a

    extnsObj = case maybeExtra of
      Nothing -> pairs codeAndPath
      -- if an `extensions` key is given in the error response from the webhook,
      -- we ignore the `code` key regardless of whether the `extensions` object
      -- contains a `code` field:
      Just (ExtraExtensions v) -> toEncoding v
      Just (ExtraInternal v) ->
        pairs $ appendIf includeInternal codeAndPath ("internal" .= v)
    codeAndPath =
      ("path" .= encodeJSONPath jPath)
        <> ("code" .= code)

-- whether internal should be included or not
encodeQErr :: Bool -> QErr -> Encoding
encodeQErr True = toEncoding
encodeQErr False = toEncoding . removeInternalErr
  where
    removeInternalErr :: QErr -> QErr
    removeInternalErr err = err {qeInternal = Nothing}

-- Postgres Connection Errors
instance PG.FromPGConnErr QErr where
  fromPGConnErr c
    | "too many clients" `T.isInfixOf` (PG.getConnErr c) =
        let e = err500 PostgresMaxConnectionsError "max connections reached on postgres"
         in e {qeInternal = Just $ ExtraInternal $ toJSON c}
    | "root certificate file" `T.isInfixOf` (PG.getConnErr c) =
        err500 PostgresError "root certificate error"
    | "certificate file" `T.isInfixOf` (PG.getConnErr c) =
        err500 PostgresError "certificate error"
    | "private key file" `T.isInfixOf` (PG.getConnErr c) =
        err500 PostgresError "private-key error"
  fromPGConnErr c =
    (err500 PostgresError "connection error")
      { qeInternal = Just $ ExtraInternal $ toJSON c
      }

-- Postgres Transaction error
instance PG.FromPGTxErr QErr where
  fromPGTxErr txe =
    (err500 PostgresError "postgres tx error")
      { qeInternal = Just $ ExtraInternal $ toJSON txe
      }

err400 :: Code -> Text -> QErr
err400 c t = QErr [] HTTP.status400 t c Nothing

err404 :: Code -> Text -> QErr
err404 c t = QErr [] HTTP.status404 t c Nothing

err405 :: Code -> Text -> QErr
err405 c t = QErr [] HTTP.status405 t c Nothing

err401 :: Code -> Text -> QErr
err401 c t = QErr [] HTTP.status401 t c Nothing

err409 :: Code -> Text -> QErr
err409 c t = QErr [] HTTP.status409 t c Nothing

err429 :: Code -> Text -> QErr
err429 c t = QErr [] HTTP.status429 t c Nothing

err500 :: Code -> Text -> QErr
err500 c t = QErr [] HTTP.status500 t c Nothing

type QErrM m = (MonadError QErr m)

throw400 :: (QErrM m) => Code -> Text -> m a
throw400 c t = throwError $ err400 c t

throw400WithDetail :: (QErrM m) => Code -> Text -> Value -> m a
throw400WithDetail c t detail =
  throwError $ (err400 c t) {qeInternal = Just $ ExtraInternal detail}

throw404 :: (QErrM m) => Text -> m a
throw404 t = throwError $ err404 NotFound t

-- | MethodNotAllowed
throw405 :: (QErrM m) => Text -> m a
throw405 t = throwError $ err405 MethodNotAllowed t

-- | AccessDenied
throw401 :: (QErrM m) => Text -> m a
throw401 t = throwError $ err401 AccessDenied t

-- | Conflict
throw409 :: (QErrM m) => Text -> m a
throw409 t = throwError $ err409 Conflict t

throw429 :: (QErrM m) => Code -> Text -> m a
throw429 c t = throwError $ err429 c t

throw500 :: (QErrM m) => Text -> m a
throw500 t = throwError $ internalError t

internalError :: Text -> QErr
internalError = err500 Unexpected

throw500WithDetail :: (QErrM m) => Text -> Value -> m a
throw500WithDetail t detail =
  throwError $ (err500 Unexpected t) {qeInternal = Just $ ExtraInternal detail}

modifyQErr ::
  (QErrM m) =>
  (QErr -> QErr) ->
  m a ->
  m a
modifyQErr f a = catchError a (throwError . f)

modifyErr ::
  (QErrM m) =>
  (Text -> Text) ->
  m a ->
  m a
modifyErr f = modifyQErr (liftTxtMod f)

modifyErrA :: (ArrowError QErr arr) => arr (e, s) a -> arr (e, (Text -> Text, s)) a
modifyErrA f = proc (e, (g, s)) -> (| mapErrorA (f -< (e, s)) |) (liftTxtMod g)

liftTxtMod :: (Text -> Text) -> QErr -> QErr
liftTxtMod f (QErr path st s c i) = QErr path st (f s) c i

modifyErrAndSet500 ::
  (QErrM m) =>
  (Text -> Text) ->
  m a ->
  m a
modifyErrAndSet500 f = modifyQErr (liftTxtMod500 f)

liftTxtMod500 :: (Text -> Text) -> QErr -> QErr
liftTxtMod500 f (QErr path _ s c i) = QErr path HTTP.status500 (f s) c i

withPathE :: (ArrowError QErr arr) => arr (e, s) a -> arr (e, (JSONPathElement, s)) a
withPathE f = proc (e, (pe, s)) -> (| mapErrorA ((e, s) >- f) |) (injectPrefix pe)
  where
    injectPrefix pe (QErr path st msg code i) = QErr (pe : path) st msg code i

withPathKA :: (ArrowError QErr arr) => arr (e, s) a -> arr (e, (Text, s)) a
withPathKA f = second (first $ arr (Key . K.fromText)) >>> withPathE f

withPathK :: (QErrM m) => Text -> m a -> m a
withPathK a = runKleisli proc m -> (| withPathKA (m >- bindA) |) a

withPathIA :: (ArrowError QErr arr) => arr (e, s) a -> arr (e, (Int, s)) a
withPathIA f = second (first $ arr Index) >>> withPathE f

withPathI :: (QErrM m) => Int -> m a -> m a
withPathI a = runKleisli proc m -> (| withPathIA (m >- bindA) |) a

indexedFoldlA' ::
  (ArrowChoice arr, ArrowError QErr arr, Foldable t) =>
  arr (e, (b, (a, s))) b ->
  arr (e, (b, (t a, s))) b
indexedFoldlA' f = proc (e, (acc0, (xs, s))) ->
  (|
    foldlA'
      (\acc (i, v) -> (| withPathIA ((e, (acc, (v, s))) >- f) |) i)
  |)
    acc0
    (zip [0 ..] (toList xs))

indexedTraverseA_ ::
  (ArrowChoice arr, ArrowError QErr arr, Foldable t) =>
  arr (e, (a, s)) b ->
  arr (e, (t a, s)) ()
indexedTraverseA_ f = proc (e, (xs, s)) ->
  (| indexedFoldlA' (\() x -> do (e, (x, s)) >- f; () >- returnA) |) () xs

indexedMapM_ :: (QErrM m, Foldable t) => (a -> m b) -> t a -> m ()
indexedMapM_ f = runKleisli proc xs -> (| indexedTraverseA_ (\x -> f x >- bindA) |) xs

indexedForM_ :: (QErrM m, Foldable t) => t a -> (a -> m b) -> m ()
indexedForM_ = flip indexedMapM_

indexedMapM :: (QErrM m) => (a -> m b) -> [a] -> m [b]
indexedMapM f = traverse (\(i, x) -> withPathI i (f x)) . zip [0 ..]

indexedForM :: (QErrM m) => [a] -> (a -> m b) -> m [b]
indexedForM = flip indexedMapM

liftIResult :: (QErrM m) => IResult a -> m a
liftIResult (IError path msg) =
  throwError $ QErr path HTTP.status400 (T.pack $ formatMsg msg) ParseFailed Nothing
liftIResult (ISuccess a) =
  return a

iResultToMaybe :: IResult a -> Maybe a
iResultToMaybe (IError _ _) = Nothing
iResultToMaybe (ISuccess a) = Just a

formatMsg :: String -> String
formatMsg str = case T.splitOn "the key " txt of
  [_, txt2] -> case T.splitOn " was not present" txt2 of
    [key, _] -> "the key '" ++ T.unpack key ++ "' was not present"
    _ -> str
  _ -> str
  where
    txt = T.pack str

runAesonParser :: (QErrM m) => (v -> Parser a) -> v -> m a
runAesonParser p =
  liftIResult . iparse p

decodeValue :: (FromJSON a, QErrM m) => Value -> m a
decodeValue = liftIResult . ifromJSON

-- Template haskell code
$(makeLensesFor [("qeInternal", "qeInternalLens")] ''QErr)
$(makePrisms ''QErrExtra)
