module Hasura.GraphQL.Resolve.Context
  ( FieldMap
  , RelationInfoMap
  , FuncArgItem(..)
  , OrdByCtx
  , OrdByItemMap
  , OrdByItem(..)
  , FuncArgSeq
  , PGColArgMap
  , UpdPermForIns(..)
  , InsCtx(..)
  , InsCtxMap
  , RespTx
  , LazyRespTx
  , AnnPGVal(..)
  , PrepFn
  , InsertTxConflictCtx(..)
  , getFldInfo
  , getPGColInfo
  , getArg
  , withArg
  , withArgM
  , nameAsPath
  , PrepArgs
  , QueryResolver(..)
  , Convert
  , runConvert
  , withPrepArgs
  , prepare
  , prepareColVal
  , txtConverter
  , module Hasura.GraphQL.Utils
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.Aeson                          as J
import qualified Data.Aeson.Casing                   as J
import qualified Data.Aeson.TH                       as J
import qualified Data.HashMap.Strict                 as Map
import qualified Data.Sequence                       as Seq
import qualified Database.PG.Query                   as Q
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.GraphQL.Resolve.ContextTypes

import           Hasura.EncJSON
import           Hasura.GraphQL.Utils
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML                      as S

data InsResp
  = InsResp
  { _irAffectedRows :: !Int
  , _irResponse     :: !(Maybe J.Object)
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''InsResp)

type RespTx = Q.TxE QErr EncJSON

type LazyRespTx = LazyTx QErr EncJSON

type PrepFn m = AnnPGVal -> m S.SQLExp

data AnnPGVal
  = AnnPGVal
  { _apvVariable   :: !(Maybe G.Variable)
  , _apvIsNullable :: !Bool
  , _apvType       :: !PGColType
  , _apvValue      :: !PGColValue
  } deriving (Show, Eq)

getFldInfo
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => G.NamedType -> G.Name
  -> m (Either PGColInfo (RelInfo, Bool, AnnBoolExpSQL, Maybe Int))
getFldInfo nt n = do
  fldMap <- asks getter
  onNothing (Map.lookup (nt,n) fldMap) $
    throw500 $ "could not lookup " <> showName n <> " in " <>
    showNamedTy nt

getPGColInfo
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => G.NamedType -> G.Name -> m PGColInfo
getPGColInfo nt n = do
  fldInfo <- getFldInfo nt n
  case fldInfo of
    Left pgColInfo -> return pgColInfo
    Right _        -> throw500 $
      "found relinfo when expecting pgcolinfo for "
      <> showNamedTy nt <> ":" <> showName n

getArg
  :: (MonadError QErr m)
  => ArgsMap
  -> G.Name
  -> m AnnInpVal
getArg args arg =
  onNothing (Map.lookup arg args) $
  throw500 $ "missing argument: " <> showName arg

prependArgsInPath
  :: (MonadError QErr m)
  => m a -> m a
prependArgsInPath = withPathK "args"

nameAsPath
  :: (MonadError QErr m)
  => G.Name -> m a -> m a
nameAsPath name = withPathK (G.unName name)

withArg
  :: (MonadError QErr m)
  => ArgsMap
  -> G.Name
  -> (AnnInpVal -> m a)
  -> m a
withArg args arg f = prependArgsInPath $ nameAsPath arg $
  getArg args arg >>= f

withArgM
  :: (MonadError QErr m)
  => ArgsMap
  -> G.Name
  -> (AnnInpVal -> m a)
  -> m (Maybe a)
withArgM args arg f = prependArgsInPath $ nameAsPath arg $
  mapM f $ handleNull =<< Map.lookup arg args
  where
    handleNull v = bool (Just v) Nothing $
                   hasNullVal $ _aivValue v

type PrepArgs = Seq.Seq Q.PrepArg

newtype QueryResolver
  = QueryResolver{getQueryResolver :: SelSet -> RespTx}

type Convert =
  (ReaderT ( FieldMap
           , OrdByCtx
           , InsCtxMap
           , SQLGenCtx
           , QueryResolver
           ) (Except QErr)
  )

prepare
  :: (MonadState PrepArgs m) => PrepFn m
prepare (AnnPGVal _ _ colTy colVal) =
  prepareColVal colTy colVal

prepareColVal
  :: (MonadState PrepArgs m)
  => PGColType -> PGColValue -> m S.SQLExp
prepareColVal colTy colVal = do
  preparedArgs <- get
  put (preparedArgs Seq.|> binEncoder colVal)
  return $ toPrepParam (Seq.length preparedArgs + 1) colTy


txtConverter :: Monad m => PrepFn m
txtConverter (AnnPGVal _ _ a b) =
  return $ toTxtValue a b

withPrepArgs :: StateT PrepArgs Convert a -> Convert (a, PrepArgs)
withPrepArgs m = runStateT m Seq.empty

runConvert
  :: (MonadError QErr m)
  => (FieldMap, OrdByCtx, InsCtxMap, SQLGenCtx, QueryResolver)
  -> Convert a
  -> m a
runConvert ctx m =
  either throwError return $
  runExcept $ runReaderT m ctx
