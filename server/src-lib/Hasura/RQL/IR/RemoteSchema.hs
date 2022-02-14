module Hasura.RQL.IR.RemoteSchema
  ( RemoteFieldArgument (..),
    RemoteSchemaSelect (..),
  )
where

import Hasura.GraphQL.Parser.Schema (InputValue)
import Hasura.Prelude
import Hasura.RQL.Types.Relationships.ToSchema
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.ResultCustomization
import Language.GraphQL.Draft.Syntax qualified as G

data RemoteFieldArgument = RemoteFieldArgument
  { _rfaArgument :: !G.Name,
    _rfaValue :: !(InputValue RemoteSchemaVariable)
  }
  deriving (Eq, Show)

data RemoteSchemaSelect = RemoteSchemaSelect
  { _rselArgs :: ![RemoteFieldArgument],
    _rselResultCustomizer :: !ResultCustomizer,
    _rselSelection :: !(G.SelectionSet G.NoFragments RemoteSchemaVariable),
    _rselFieldCall :: !(NonEmpty FieldCall),
    _rselRemoteSchema :: !RemoteSchemaInfo
  }
