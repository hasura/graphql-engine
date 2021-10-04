module Hasura.GraphQL.Schema.RemoteJoin where

import Hasura.GraphQL.Parser
import Hasura.GraphQL.Schema.Backend
import Hasura.GraphQL.Schema.Common
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.Types.Common
import Hasura.RQL.Types.Relationships.FromSchema
import Hasura.RQL.Types.Relationships.FromSource
import Hasura.SQL.AnyBackend

sourceToSourceField ::
  forall b r m n.
  MonadBuildSchema b r m n =>
  AnyBackend (RemoteSourceFieldInfo b) ->
  m [FieldParser n (AnnotatedField b)]
schemaToSourceField ::
  forall r m n.
  MonadBuildSchemaBase r m n =>
  RelName ->
  AnyBackend ResolvedFromSchemaToSourceRelationship ->
  m [FieldParser n (IR.SchemaRelationshipSelect UnpreparedValue)]
