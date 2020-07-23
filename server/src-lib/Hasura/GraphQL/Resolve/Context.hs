module Hasura.GraphQL.Resolve.Context
  ( FunctionArgItem(..)
  , OrdByItem(..)
  , UpdPermForIns(..)
  , InsCtx(..)
  , RespTx
  , LazyRespTx
  , AnnPGVal(..)
  , UnresolvedVal(..)
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

  , traverseObjectSelectionSet
  , fieldAsPath
  , resolvePGCol
  , module Hasura.GraphQL.Utils
  , module Hasura.GraphQL.Resolve.Types
  ) where

import           Data.Has
import           Hasura.Prelude

import qualified Data.HashMap.Strict                  as Map
import qualified Data.Sequence                        as Seq
import qualified Database.PG.Query                    as Q
import qualified Language.GraphQL.Draft.Syntax        as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Utils
import           Hasura.GraphQL.Validate.SelectionSet
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.DML.Internal              (currentSession, sessVarFromCurrentSetting)
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import qualified Hasura.SQL.DML                       as S

getFldInfo
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => G.NamedType -> G.Name
  -> m ResolveField
getFldInfo nt n = do
  fldMap <- asks getter
  onNothing (Map.lookup (nt,n) fldMap) $
    throw500 $ "could not lookup " <> showName n <> " in " <>
    showNamedTy nt

getPGColInfo
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => G.NamedType -> G.Name -> m PGColumnInfo
getPGColInfo nt n = do
  fldInfo <- getFldInfo nt n
  case fldInfo of
    RFPGColumn pgColInfo   -> return pgColInfo
    RFRelationship _       -> throw500 $ mkErrMsg "relation"
    RFComputedField _      -> throw500 $ mkErrMsg "computed field"
    RFRemoteRelationship _ -> throw500 $ mkErrMsg "remote relationship"
    RFNodeId _ _           -> throw500 $ mkErrMsg "node id"
  where
    mkErrMsg ty =
      "found " <> ty <> " when expecting pgcolinfo for "
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
  :: (MonadReusability m, MonadError QErr m)
  => ArgsMap
  -> G.Name
  -> (AnnInpVal -> m a)
  -> m (Maybe a)
withArgM args argName f = do
  wrappedArg <- for (Map.lookup argName args) $ \arg -> do
    when (isJust (_aivVariable arg) && G.isNullable (_aivType arg)) markNotReusable
    pure . bool (Just arg) Nothing $ hasNullVal (_aivValue arg)
  prependArgsInPath . nameAsPath argName $ traverse f (join wrappedArg)

type PrepArgs = Seq.Seq Q.PrepArg

prepare :: (MonadState PrepArgs m) => AnnPGVal -> m S.SQLExp
prepare (AnnPGVal _ _ scalarValue) = prepareColVal scalarValue

resolveValTxt :: (Applicative f) => UnresolvedVal -> f S.SQLExp
resolveValTxt = \case
  UVPG annPGVal -> txtConverter annPGVal
  UVSessVar colTy sessVar -> sessVarFromCurrentSetting colTy sessVar
  UVSQL sqlExp -> pure sqlExp
  UVSession -> pure currentSession

withPrepArgs :: StateT PrepArgs m a -> m (a, PrepArgs)
withPrepArgs m = runStateT m Seq.empty

prepareColVal
  :: (MonadState PrepArgs m)
  => WithScalarType PGScalarValue -> m S.SQLExp
prepareColVal (WithScalarType scalarType colVal) = do
  preparedArgs <- get
  put (preparedArgs Seq.|> binEncoder colVal)
  return $ toPrepParam (Seq.length preparedArgs + 1) scalarType

txtConverter :: Applicative f => AnnPGVal -> f S.SQLExp
txtConverter (AnnPGVal _ _ scalarValue) = pure $ toTxtValue scalarValue

fieldAsPath :: (MonadError QErr m) => Field -> m a -> m a
fieldAsPath = nameAsPath . _fName

resolvePGCol :: (MonadError QErr m)
             => PGColGNameMap -> G.Name -> m PGColumnInfo
resolvePGCol colFldMap fldName =
  onNothing (Map.lookup fldName colFldMap) $ throw500 $
  "no column associated with name " <> G.unName fldName
