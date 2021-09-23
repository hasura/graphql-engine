module Hasura.GraphQL.Schema.RemoteSource where

import Hasura.GraphQL.Parser
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.RQL.Types.RemoteRelationship
import Hasura.SQL.AnyBackend

remoteSourceField ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  AnyBackend (RemoteSourceRelationshipInfo b) ->
  m [FieldParser n (AnnotatedField b)]
