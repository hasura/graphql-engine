module Hasura.GraphQL.Resolve.Context
  ( FuncArgItem(..)
  , OrdByItem(..)
  , UpdPermForIns(..)
  , InsCtx(..)
  , RespTx
  , LazyRespTx
  , AnnPGVal(..)
  , UnresolvedVal(..)
  , resolveValPrep
  , resolveValTxt
  , InsertTxConflictCtx(..)
  , getFldInfo
  , getPGColInfo
  , getArg
  , withArg
  , withArgM
  , nameAsPath

  , PrepArgs
  , prepare
  , prepareColVal
  , withPrepArgs

  , txtConverter

  , withSelSet
  , fieldAsPath
  , module Hasura.GraphQL.Utils
  , module Hasura.GraphQL.Resolve.Types
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict           as Map
import qualified Data.Sequence                 as Seq
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Utils
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal       (sessVarFromCurrentSetting)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML                as S

getFldInfo
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => G.NamedType -> G.Name
  -> m TypedField
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
    FldCol pgColInfo -> return pgColInfo
    _ -> throw500 $
      "expecting pgcolinfo for "
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

prepare
  :: (MonadState PrepArgs m) => AnnPGVal -> m S.SQLExp
prepare (AnnPGVal _ _ colTy colVal) =
  prepareColVal colTy colVal

resolveValPrep
  :: (MonadState PrepArgs m)
  => UnresolvedVal -> m S.SQLExp
resolveValPrep = \case
  UVPG annPGVal -> prepare annPGVal
  UVSessVar colTy sessVar -> sessVarFromCurrentSetting colTy sessVar
  UVSQL sqlExp -> return sqlExp

resolveValTxt :: (Applicative f) => UnresolvedVal -> f S.SQLExp
resolveValTxt = \case
  UVPG annPGVal -> txtConverter annPGVal
  UVSessVar colTy sessVar -> sessVarFromCurrentSetting colTy sessVar
  UVSQL sqlExp -> pure sqlExp

withPrepArgs :: StateT PrepArgs m a -> m (a, PrepArgs)
withPrepArgs m = runStateT m Seq.empty

prepareColVal
  :: (MonadState PrepArgs m)
  => PGColType -> PGColValue -> m S.SQLExp
prepareColVal colTy colVal = do
  preparedArgs <- get
  put (preparedArgs Seq.|> binEncoder colVal)
  return $ toPrepParam (Seq.length preparedArgs + 1) colTy

txtConverter :: Applicative f => AnnPGVal -> f S.SQLExp
txtConverter (AnnPGVal _ _ a b) =
  pure $ toTxtValue a b

withSelSet :: (Monad m) => SelSet -> (Field -> m a) -> m [(Text, a)]
withSelSet selSet f =
  forM (toList selSet) $ \fld -> do
    res <- f fld
    return (G.unName $ G.unAlias $ _fAlias fld, res)

fieldAsPath :: (MonadError QErr m) => Field -> m a -> m a
fieldAsPath = nameAsPath . _fName
