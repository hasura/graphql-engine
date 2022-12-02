module Hasura.GraphQL.Parser.Names (HasName (..)) where

import Language.GraphQL.Draft.Syntax (Name)

class HasName a where
  getName :: a -> Name

instance HasName Name where
  getName = id
