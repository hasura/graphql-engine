module Hasura.GraphQL.Parser.Class where

import Data.Kind (Type)

class MonadParse (m :: Type -> Type)
