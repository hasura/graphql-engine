{-# LANGUAGE UndecidableInstances #-}

module Hasura.LogicalModel.Fields (LogicalModelFieldsRM (..), LogicalModelFieldsLookupRT (..), runLogicalModelFieldsLookup) where

import Data.HashMap.Strict qualified as HashMap
import Hasura.Backends.Postgres.Connection qualified as Postgres
import Hasura.LogicalModel.Types
import Hasura.Prelude
import Hasura.RQL.Types.Session (UserInfoM)
import Hasura.Tracing (TraceT)

-- | Class for looking up Logical Models
class (Monad m) => LogicalModelFieldsRM b m where
  lookupLogicalModelFields :: LogicalModelName -> m (Maybe (LogicalModelFields b))
  getLogicalModelFieldsLookup :: m (LogicalModelFieldsLookup b)

instance (LogicalModelFieldsRM b m) => LogicalModelFieldsRM b (ReaderT r m) where
  lookupLogicalModelFields = lift . lookupLogicalModelFields
  getLogicalModelFieldsLookup = lift getLogicalModelFieldsLookup

instance (LogicalModelFieldsRM b m) => LogicalModelFieldsRM b (StateT s m) where
  lookupLogicalModelFields = lift . lookupLogicalModelFields
  getLogicalModelFieldsLookup = lift getLogicalModelFieldsLookup

instance (Monoid w, LogicalModelFieldsRM b m) => LogicalModelFieldsRM b (WriterT w m) where
  lookupLogicalModelFields = lift . lookupLogicalModelFields
  getLogicalModelFieldsLookup = lift getLogicalModelFieldsLookup

instance (LogicalModelFieldsRM b m) => LogicalModelFieldsRM b (TraceT m) where
  lookupLogicalModelFields = lift . lookupLogicalModelFields
  getLogicalModelFieldsLookup = lift getLogicalModelFieldsLookup

type LogicalModelFieldsLookup b = LogicalModelName -> Maybe (LogicalModelFields b)

toLogicalModelFieldsLookup :: (a -> LogicalModelFields b) -> HashMap LogicalModelName a -> LogicalModelFieldsLookup b
toLogicalModelFieldsLookup f h logicalModelName = f <$> HashMap.lookup logicalModelName h

newtype LogicalModelFieldsLookupRT b m a = LogicalModelFieldsLookupRT {runLogicalModelFieldsLookupRT :: LogicalModelFieldsLookup b -> m a}
  deriving
    (Functor, Applicative, Monad, MonadIO, MonadError e, MonadState s, MonadWriter w, Postgres.MonadTx, UserInfoM)
    via (ReaderT (LogicalModelFieldsLookup b) m)
  deriving (MonadTrans) via (ReaderT (LogicalModelFieldsLookup b))

runLogicalModelFieldsLookup :: (x -> LogicalModelFields b) -> HashMap LogicalModelName x -> LogicalModelFieldsLookupRT b m a -> m a
runLogicalModelFieldsLookup f h m =
  runLogicalModelFieldsLookupRT m $ toLogicalModelFieldsLookup f h

instance (MonadReader r m) => MonadReader r (LogicalModelFieldsLookupRT b m) where
  ask = lift ask
  local f m = LogicalModelFieldsLookupRT (local f . runLogicalModelFieldsLookupRT m)

instance (Monad m) => LogicalModelFieldsRM b (LogicalModelFieldsLookupRT b m) where
  lookupLogicalModelFields logicalModelName =
    LogicalModelFieldsLookupRT $ pure . ($ logicalModelName)
  getLogicalModelFieldsLookup = LogicalModelFieldsLookupRT pure
