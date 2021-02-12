module Hasura.GraphQL.Transport.Backend where

import           Hasura.Prelude

import qualified Language.GraphQL.Draft.Syntax          as G

import qualified Hasura.Logging                         as L

import           Hasura.EncJSON
import           Hasura.GraphQL.Execute.Backend
import           Hasura.GraphQL.Logging                 (MonadQueryLog (..))
import           Hasura.GraphQL.Transport.HTTP.Protocol
import           Hasura.RQL.Types.Common
import           Hasura.RQL.Types.Error
import           Hasura.SQL.Backend
import           Hasura.Server.Types                    (RequestId)
import           Hasura.Session
import           Hasura.Tracing


-- | This typeclass enacapsulates how a given backend sends queries and mutations over the
-- network. Each backend is currently responsible for both logging and tracing, for now.
class BackendExecute b tx => BackendTransport (b :: BackendType) tx where
  runDBQuery
    :: forall m
     . ( MonadIO m
       , MonadError QErr m
       , MonadQueryLog m
       , MonadTrace m
       )
    => RequestId
    -> GQLReqUnparsed
    -> G.Name
    -> UserInfo
    -> L.Logger L.Hasura
    -> SourceConfig b
    -> tx EncJSON
    -> Maybe (PreparedQuery b)
    -> m (DiffTime, EncJSON)
  runDBMutation
    :: forall m
     . ( MonadIO m
       , MonadError QErr m
       , MonadQueryLog m
       , MonadTrace m
       )
    => RequestId
    -> GQLReqUnparsed
    -> G.Name
    -> UserInfo
    -> L.Logger L.Hasura
    -> SourceConfig b
    -> tx EncJSON
    -> Maybe (PreparedQuery b)
    -> m (DiffTime, EncJSON)
