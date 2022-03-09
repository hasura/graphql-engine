module Hasura.RQL.IR.Action
  ( ActionFieldG (..),
    ActionFieldsG,
    ActionFields,
    ActionRemoteRelationshipSelect (..),
    _ACFExpression,
    _ACFNestedObject,
    _ACFRemote,
    _ACFScalar,
  )
where

import Control.Lens.TH (makePrisms)
import Data.Kind (Type)
import Hasura.Prelude
import Hasura.RQL.Types.Common (FieldName, Fields)
import Language.GraphQL.Draft.Syntax qualified as G

data ActionFieldG (r :: Type)
  = ACFScalar G.Name
  | ACFRemote (ActionRemoteRelationshipSelect r)
  | ACFExpression Text
  | ACFNestedObject G.Name !(ActionFieldsG r)
  deriving (Eq, Show, Functor, Foldable, Traversable)

type ActionFieldsG r = Fields (ActionFieldG r)

type ActionFields = ActionFieldsG Void

data ActionRemoteRelationshipSelect r = ActionRemoteRelationshipSelect
  { -- | The fields on the table that are required for the join condition
    -- of the remote relationship
    _arrsLHSJoinFields :: HashMap FieldName G.Name,
    -- | The field that captures the relationship
    -- r ~ (RemoteRelationshipField UnpreparedValue) when the AST is emitted by the parser.
    -- r ~ Void when an execution tree is constructed so that a backend is
    -- absolved of dealing with remote relationships.
    _arrsRelationship :: r
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

$(makePrisms ''ActionFieldG)
