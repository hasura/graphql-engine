{-# LANGUAGE TemplateHaskell #-}

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

-- | Internal representation for a selection of fields on the result of an action.
-- Type parameter r will be either
-- r ~ (RemoteRelationshipField UnpreparedValue) when the AST is emitted by the parser.
-- r ~ Void when an execution tree is constructed so that a backend is
-- absolved of dealing with remote relationships.
data ActionFieldG (r :: Type)
  = -- | Scalar value. G.Name is the original field name from the object type.
    ACFScalar G.Name
  | -- | Remote relationship
    ACFRemote (ActionRemoteRelationshipSelect r)
  | -- | Constant text value (used for __typename fields)
    ACFExpression Text
  | -- | Nested object. G.Name is the original field name from the object type.
    ACFNestedObject G.Name !(ActionFieldsG r)
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
