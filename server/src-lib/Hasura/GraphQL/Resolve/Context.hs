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
  , UnresolvedVal(..)
  , resolveValPrep
  , resolveValTxt
  , AnnBoolExpUnresolved
  , partialSQLExpToUnresolvedVal
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

import           Hasura.GraphQL.Utils
import           Hasura.GraphQL.Validate.Field
import           Hasura.GraphQL.Validate.Types
import           Hasura.RQL.Types
import           Hasura.SQL.Types
import           Hasura.SQL.Value

import           Hasura.RQL.DML.Internal             (sessVarFromCurrentSetting)

import qualified Hasura.SQL.DML                      as S

data InsResp
  = InsResp
  { _irAffectedRows :: !Int
  , _irResponse     :: !(Maybe J.Object)
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''InsResp)

data AnnPGVal
  = AnnPGVal
  { _apvVariable   :: !(Maybe G.Variable)
  , _apvIsNullable :: !Bool
  , _apvType       :: !PGColType
  , _apvValue      :: !PGColValue
  } deriving (Show, Eq)

type PrepFn m = AnnPGVal -> m S.SQLExp

-- lifts PartialSQLExp to UnresolvedVal
partialSQLExpToUnresolvedVal :: PartialSQLExp -> UnresolvedVal
partialSQLExpToUnresolvedVal = \case
  PSESessVar ty sessVar -> UVSessVar ty sessVar
  PSESQLExp s           -> UVSQL s

-- A value that will be converted to an sql expression eventually
data UnresolvedVal
  -- From a session variable
  = UVSessVar !PgType !SessVar
  -- This is postgres
  | UVPG !AnnPGVal
  -- This is a full resolved sql expression
  | UVSQL !S.SQLExp
  deriving (Show, Eq)

type AnnBoolExpUnresolved = AnnBoolExp UnresolvedVal

getFldInfo
  :: (MonadError QErr m, MonadReader r m, Has FieldMap r)
  => G.NamedType -> G.Name
  -> m (Either PGColInfo (RelInfo, Bool, AnnBoolExpPartialSQL, Maybe Int))
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
