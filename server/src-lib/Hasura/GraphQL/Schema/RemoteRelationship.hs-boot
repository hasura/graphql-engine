module Hasura.GraphQL.Schema.RemoteRelationship
  ( remoteRelationshipField,
  )
where

import Hasura.GraphQL.Parser
import Hasura.GraphQL.Schema.Common
import Hasura.Prelude
import Hasura.RQL.IR.Root qualified as IR
import Hasura.RQL.Types.Relationships.Remote

remoteRelationshipField ::
  forall r m n lhsJoinField.
  (MonadBuildSchemaBase r m n) =>
  RemoteFieldInfo lhsJoinField ->
  m (Maybe [FieldParser n (IR.RemoteRelationshipField UnpreparedValue)])
