{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.Types.Error
       ( Code(..)
       , QErr(..)
       , encodeQErr
       , nonAdminQErrEnc
       , err400
       , err401
       , err500

       , QErrM
       , throw400
       , throw404
       , throw500
       , throw401

         -- Aeson helpers
       , runAesonParser
       , decodeValue

         -- Modify error messages
       , modifyErr
       , modifyErrAndSet500

         -- Attach context
       , withPathK
       , withPathI
       , indexedFoldM
       , indexedForM
       , indexedForM_
       ) where

import           Data.Aeson
import           Data.Aeson.Internal
import           Data.Aeson.Types
import qualified Database.PG.Query    as Q
import           Hasura.Prelude
import           Text.Show            (Show (..))

import qualified Data.Text            as T
import qualified Network.HTTP.Types   as N

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
  -- Graphql error
  | NoTables
  | ValidationFailed
  deriving (Eq)

instance Show Code where
  show NotNullViolation  = "not-null-violation"
  show PermissionDenied  = "permission-denied"
  show NotExists         = "not-exists"
  show AlreadyExists     = "already-exists"
  show AlreadyTracked    = "already-tracked"
  show AlreadyUntracked  = "already-untracked"
  show PostgresError     = "postgres-error"
  show NotSupported      = "not-supported"
  show DependencyError   = "dependency-error"
  show InvalidHeaders    = "invalid-headers"
  show InvalidJSON       = "invalid-json"
  show AccessDenied      = "access-denied"
  show ParseFailed       = "parse-failed"
  show ConstraintError   = "constraint-error"
  show PermissionError   = "permission-error"
  show NotFound          = "not-found"
  show Unexpected        = "unexpected"
  show UnexpectedPayload = "unexpected-payload"
  show NoUpdate          = "no-update"
  show InvalidParams     = "invalid-params"
  show AlreadyInit       = "already-initialised"
  show NoTables          = "no-tables"
  show ValidationFailed  = "validation-failed"

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

nonAdminQErrEnc :: QErr -> Value
nonAdminQErrEnc (QErr jPath _ msg code _) =
  object
  [ "path"  .= encodeJSONPath jPath
  , "error" .= msg
  , "code"  .= show code
  ]

encodeQErr :: T.Text -> QErr -> Value
encodeQErr "admin" = toJSON
encodeQErr _       = nonAdminQErrEnc

encodeJSONPath :: JSONPath -> String
encodeJSONPath = format "$"
  where
    format pfx []                = pfx
    format pfx (Index idx:parts) = format (pfx ++ "[" ++ show idx ++ "]") parts
    format pfx (Key key:parts)   = format (pfx ++ "." ++ formatKey key) parts

    formatKey key
      | T.any (=='.') key = "['" ++ T.unpack key ++ "']"
      | otherwise         = T.unpack key

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

throw404 :: (QErrM m) => Code -> T.Text -> m a
throw404 c t = throwError $ err404 c t

throw401 :: (QErrM m) => T.Text -> m a
throw401 t = throwError $ err401 AccessDenied t

throw500 :: (QErrM m) => T.Text -> m a
throw500 t = throwError $ err500 Unexpected t

modifyQErr :: (QErrM m)
           => (QErr -> QErr) -> m a -> m a
modifyQErr f a = catchError a (throwError . f)

modifyErr :: (QErrM m)
          => (T.Text -> T.Text)
          -> m a -> m a
modifyErr f = modifyQErr (liftTxtMod f)

liftTxtMod :: (T.Text -> T.Text) -> QErr -> QErr
liftTxtMod f (QErr path st s c i) = QErr path st (f s) c i

modifyErrAndSet500 :: (QErrM m)
                   => (T.Text -> T.Text)
                   -> m a -> m a
modifyErrAndSet500 f = modifyQErr (liftTxtMod500 f)

liftTxtMod500 :: (T.Text -> T.Text) -> QErr -> QErr
liftTxtMod500 f (QErr path _ s c i) = QErr path N.status500 (f s) c i

withPathE :: (QErrM m)
          => JSONPathElement -> m a -> m a
withPathE pe m =
  catchError m (throwError . injectPrefix)
  where
    injectPrefix (QErr path st msg code i) = QErr (pe:path) st msg code i

withPathK :: (QErrM m)
          => T.Text -> m a -> m a
withPathK = withPathE . Key

withPathI :: (QErrM m)
          => Int -> m a -> m a
withPathI = withPathE . Index

indexedFoldM :: (QErrM m)
             => (b -> a -> m b)
             -> b -> [a] -> m b
indexedFoldM f b al =
  foldM f' b $ zip [0..] al
  where
    f' accum (i, a) = withPathE (Index i) (f accum a)

indexedForM :: (QErrM m)
            => [a] -> (a -> m b) -> m [b]
indexedForM l f =
  forM (zip [0..] l) $ \(i, a) ->
    withPathE (Index i) (f a)

indexedForM_ :: (QErrM m)
             => [a] -> (a -> m ()) -> m ()
indexedForM_ l f =
  forM_ (zip [0..] l) $ \(i, a) ->
    withPathE (Index i) (f a)

liftIResult :: (QErrM m) => IResult a -> m a
liftIResult (IError path msg) =
  throwError $ QErr path N.status400 (T.pack $ formatMsg msg) ParseFailed Nothing
liftIResult (ISuccess a) =
  return a

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
