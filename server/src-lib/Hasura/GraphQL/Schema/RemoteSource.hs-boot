module Hasura.GraphQL.Schema.RemoteSource
  ( remoteSourceField,
  )
where

import Hasura.GraphQL.Parser
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.RQL.Types.Relationships.FromSource
import Hasura.SQL.AnyBackend

remoteSourceField ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  AnyBackend (RemoteSourceFieldInfo b) ->
  m [FieldParser n (AnnotatedField b)]
