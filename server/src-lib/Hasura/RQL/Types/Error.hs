{-# LANGUAGE Arrows #-}

module Hasura.RQL.Types.Error
       ( Code(..)
       , QErr(..)
       , encodeJSONPath
       , encodeQErr
       , encodeGQLErr
       , encodeJSONPath
       , noInternalQErrEnc
       , err400
       , err404
       , err401
       , err500
       , internalError

       , QErrM
       , throw400
       , throw404
       , throw500
       , throw500WithDetail
       , throw401

       , iResultToMaybe

         -- Aeson helpers
       , runAesonParser
       , decodeValue

         -- Modify error messages
       , modifyErr
       , modifyErrAndSet500
       , modifyQErr
       , modifyErrA

         -- Attach context
       , withPathK
       , withPathKA
       , withPathI
       , withPathIA
       , indexedFoldM
       , indexedFoldlA'
       , indexedForM
       , indexedMapM
       , indexedTraverseA
       , indexedForM_
       , indexedMapM_
       , indexedTraverseA_
       ) where

import           Control.Arrow.Extended
import           Data.Aeson
import           Data.Aeson.Internal
import           Data.Aeson.Types
import qualified Database.PG.Query      as Q
import           Hasura.Prelude
import           Text.Show              (Show (..))

import qualified Data.Text              as T
import qualified Network.HTTP.Types     as N

data Code
  = PermissionDenied
  | NotNullViolation
  | NotExists
  | AlreadyExists
  | PostgresError
  | NotSupported
  | DependencyError
  | InvalidHeaders
  | InvalidJSON
  | AccessDenied
  | ParseFailed
  | ConstraintError
  | PermissionError
  | NotFound
  | Unexpected
  | UnexpectedPayload
  | NoUpdate
  | AlreadyTracked
  | AlreadyUntracked
  | InvalidParams
  | AlreadyInit
  | ConstraintViolation
  | DataException
  | BadRequest
  -- Graphql error
  | NoTables
  | ValidationFailed
  | Busy
  -- JWT Auth errors
  | JWTRoleClaimMissing
  | JWTInvalidClaims
  | JWTInvalid
  | JWTInvalidKey
  -- Remote schemas
  | RemoteSchemaError
  | RemoteSchemaConflicts
  -- Websocket/Subscription errors
  | StartFailed
  | InvalidCustomTypes
  -- Actions Webhook code
  | ActionWebhookCode !Text
  deriving (Eq)

instance Show Code where
  show = \case
    NotNullViolation      -> "not-null-violation"
    DataException         -> "data-exception"
    BadRequest            -> "bad-request"
    ConstraintViolation   -> "constraint-violation"
    PermissionDenied      -> "permission-denied"
    NotExists             -> "not-exists"
    AlreadyExists         -> "already-exists"
    AlreadyTracked        -> "already-tracked"
    AlreadyUntracked      -> "already-untracked"
    PostgresError         -> "postgres-error"
    NotSupported          -> "not-supported"
    DependencyError       -> "dependency-error"
    InvalidHeaders        -> "invalid-headers"
    InvalidJSON           -> "invalid-json"
    AccessDenied          -> "access-denied"
    ParseFailed           -> "parse-failed"
    ConstraintError       -> "constraint-error"
    PermissionError       -> "permission-error"
    NotFound              -> "not-found"
    Unexpected            -> "unexpected"
    UnexpectedPayload     -> "unexpected-payload"
    NoUpdate              -> "no-update"
    InvalidParams         -> "invalid-params"
    AlreadyInit           -> "already-initialised"
    NoTables              -> "no-tables"
    ValidationFailed      -> "validation-failed"
    Busy                  -> "busy"
    JWTRoleClaimMissing   -> "jwt-missing-role-claims"
    JWTInvalidClaims      -> "jwt-invalid-claims"
    JWTInvalid            -> "invalid-jwt"
    JWTInvalidKey         -> "invalid-jwt-key"
    RemoteSchemaError     -> "remote-schema-error"
    RemoteSchemaConflicts -> "remote-schema-conflicts"
    StartFailed           -> "start-failed"
    InvalidCustomTypes    -> "invalid-custom-types"
    ActionWebhookCode t   -> T.unpack t

data QErr
  = QErr
  { qePath     :: !JSONPath
  , qeStatus   :: !N.Status
  , qeError    :: !T.Text
  , qeCode     :: !Code
  , qeInternal :: !(Maybe Value)
  } deriving (Show, Eq)

instance ToJSON QErr where
  toJSON (QErr jPath _ msg code Nothing) =
    object
    [ "path"  .= encodeJSONPath jPath
    , "error" .= msg
    , "code"  .= show code
    ]
  toJSON (QErr jPath _ msg code (Just ie)) =
    object
    [ "path"  .= encodeJSONPath jPath
    , "error" .= msg
    , "code"  .= show code
    , "internal" .= ie
    ]

noInternalQErrEnc :: QErr -> Value
noInternalQErrEnc (QErr jPath _ msg code _) =
  object
  [ "path"  .= encodeJSONPath jPath
  , "error" .= msg
  , "code"  .= show code
  ]

encodeGQLErr :: Bool -> QErr -> Value
encodeGQLErr includeInternal (QErr jPath _ msg code mIE) =
  object
  [ "message" .= msg
  , "extensions" .= extnsObj
  ]
  where
    extnsObj = object $ bool codeAndPath
               (codeAndPath ++ internal) includeInternal
    codeAndPath = [ "code" .= show code
                  , "path" .= encodeJSONPath jPath
                  ]
    internal = maybe [] (\ie -> ["internal" .= ie]) mIE

-- whether internal should be included or not
encodeQErr :: Bool -> QErr -> Value
encodeQErr True = toJSON
encodeQErr _    = noInternalQErrEnc

encodeJSONPath :: JSONPath -> String
encodeJSONPath = format "$"
  where
    format pfx []                = pfx
    format pfx (Index idx:parts) = format (pfx ++ "[" ++ show idx ++ "]") parts
    format pfx (Key key:parts)   = format (pfx ++ formatKey key) parts

    formatKey key
      | specialChars sKey = "['" ++ sKey ++ "']"
      | otherwise         = "." ++ sKey
      where
        sKey = T.unpack key
        specialChars []     = True
        -- first char must not be number
        specialChars (c:xs) = notElem c (alphabet ++ "_") ||
          any (flip notElem (alphaNumerics ++ "_-")) xs

instance Q.FromPGConnErr QErr where
  fromPGConnErr c =
    let e = err500 PostgresError "connection error"
    in e {qeInternal = Just $ toJSON c}

instance Q.FromPGTxErr QErr where
  fromPGTxErr txe =
    let e = err500 PostgresError "postgres tx error"
    in e {qeInternal = Just $ toJSON txe}

err400 :: Code -> T.Text -> QErr
err400 c t = QErr [] N.status400 t c Nothing

err404 :: Code -> T.Text -> QErr
err404 c t = QErr [] N.status404 t c Nothing

err401 :: Code -> T.Text -> QErr
err401 c t = QErr [] N.status401 t c Nothing

err500 :: Code -> T.Text -> QErr
err500 c t = QErr [] N.status500 t c Nothing

type QErrM m = (MonadError QErr m)

throw400 :: (QErrM m) => Code -> T.Text -> m a
throw400 c t = throwError $ err400 c t

throw404 :: (QErrM m) => T.Text -> m a
throw404 t = throwError $ err404 NotFound t

throw401 :: (QErrM m) => T.Text -> m a
throw401 t = throwError $ err401 AccessDenied t

throw500 :: (QErrM m) => T.Text -> m a
throw500 t = throwError $ internalError t

internalError :: Text -> QErr
internalError = err500 Unexpected

throw500WithDetail :: (QErrM m) => T.Text -> Value -> m a
throw500WithDetail t detail =
  throwError $ (err500 Unexpected t) {qeInternal = Just detail}

modifyQErr :: (QErrM m)
           => (QErr -> QErr) -> m a -> m a
modifyQErr f a = catchError a (throwError . f)

modifyErr :: (QErrM m)
          => (T.Text -> T.Text)
          -> m a -> m a
modifyErr f = modifyQErr (liftTxtMod f)

modifyErrA :: (ArrowError QErr arr) => arr (e, s) a -> arr (e, (Text -> Text, s)) a
modifyErrA f = proc (e, (g, s)) -> (| mapErrorA (f -< (e, s)) |) (liftTxtMod g)

liftTxtMod :: (T.Text -> T.Text) -> QErr -> QErr
liftTxtMod f (QErr path st s c i) = QErr path st (f s) c i

modifyErrAndSet500 :: (QErrM m)
                   => (T.Text -> T.Text)
                   -> m a -> m a
modifyErrAndSet500 f = modifyQErr (liftTxtMod500 f)

liftTxtMod500 :: (T.Text -> T.Text) -> QErr -> QErr
liftTxtMod500 f (QErr path _ s c i) = QErr path N.status500 (f s) c i

withPathE :: (ArrowError QErr arr) => arr (e, s) a -> arr (e, (JSONPathElement, s)) a
withPathE f = proc (e, (pe, s)) -> (| mapErrorA ((e, s) >- f) |) (injectPrefix pe)
  where
    injectPrefix pe (QErr path st msg code i) = QErr (pe:path) st msg code i

withPathKA :: (ArrowError QErr arr) => arr (e, s) a -> arr (e, (Text, s)) a
withPathKA f = second (first $ arr Key) >>> withPathE f

withPathK :: (QErrM m) => Text -> m a -> m a
withPathK a = runKleisli proc m -> (| withPathKA (m >- bindA) |) a

withPathIA :: (ArrowError QErr arr) => arr (e, s) a -> arr (e, (Int, s)) a
withPathIA f = second (first $ arr Index) >>> withPathE f

withPathI :: (QErrM m) => Int -> m a -> m a
withPathI a = runKleisli proc m -> (| withPathIA (m >- bindA) |) a

indexedFoldlA'
  :: (ArrowChoice arr, ArrowError QErr arr, Foldable t)
  => arr (e, (b, (a, s))) b -> arr (e, (b, (t a, s))) b
indexedFoldlA' f = proc (e, (acc0, (xs, s))) ->
  (| foldlA' (\acc (i, v) -> (| withPathIA ((e, (acc, (v, s))) >- f) |) i)
  |) acc0 (zip [0..] (toList xs))

indexedFoldM :: (QErrM m, Foldable t) => (b -> a -> m b) -> b -> t a -> m b
indexedFoldM f acc0 = runKleisli proc xs ->
  (| indexedFoldlA' (\acc v -> f acc v >- bindA) |) acc0 xs

indexedTraverseA_
  :: (ArrowChoice arr, ArrowError QErr arr, Foldable t)
  => arr (e, (a, s)) b -> arr (e, (t a, s)) ()
indexedTraverseA_ f = proc (e, (xs, s)) ->
  (| indexedFoldlA' (\() x -> do { (e, (x, s)) >- f; () >- returnA }) |) () xs

indexedMapM_ :: (QErrM m, Foldable t) => (a -> m b) -> t a -> m ()
indexedMapM_ f = runKleisli proc xs -> (| indexedTraverseA_ (\x -> f x >- bindA) |) xs

indexedForM_ :: (QErrM m, Foldable t) => t a -> (a -> m b) -> m ()
indexedForM_ = flip indexedMapM_

indexedTraverseA
  :: (ArrowChoice arr, ArrowError QErr arr)
  => arr (e, (a, s)) b -> arr (e, ([a], s)) [b]
indexedTraverseA f = proc (e, (xs, s)) ->
  (| traverseA (\(i, x) -> (| withPathIA ((e, (x, s)) >- f) |) i)
  |) (zip [0..] (toList xs))

indexedMapM :: (QErrM m) => (a -> m b) -> [a] -> m [b]
indexedMapM f = traverse (\(i, x) -> withPathI i (f x)) . zip [0..]

indexedForM :: (QErrM m) => [a] -> (a -> m b) -> m [b]
indexedForM = flip indexedMapM

liftIResult :: (QErrM m) => IResult a -> m a
liftIResult (IError path msg) =
  throwError $ QErr path N.status400 (T.pack $ formatMsg msg) ParseFailed Nothing
liftIResult (ISuccess a) =
  return a

iResultToMaybe :: IResult a -> Maybe a
iResultToMaybe (IError _ _) = Nothing
iResultToMaybe (ISuccess a) = Just a

formatMsg :: String -> String
formatMsg str = case T.splitOn "the key " txt of
  [_, txt2] -> case T.splitOn " was not present" txt2 of
                 [key, _] -> "the key '" ++ T.unpack key ++ "' was not present"
                 _        -> str
  _         -> str
  where
    txt = T.pack str

runAesonParser :: (QErrM m) => (Value -> Parser a) -> Value -> m a
runAesonParser p =
  liftIResult . iparse p

decodeValue :: (FromJSON a, QErrM m) => Value -> m a
decodeValue = liftIResult . ifromJSON
