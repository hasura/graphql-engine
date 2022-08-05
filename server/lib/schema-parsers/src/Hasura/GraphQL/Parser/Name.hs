-- | Re-exports everything from the sub-modules.
--
-- Submodules contain names relevant to the GraphQL specification, October 2021.
-- https://spec.graphql.org/October2021/
--
-- Each name is placed in a Haddock section corresponding to its section in the specification.
module Hasura.GraphQL.Parser.Name
  ( module Hasura.GraphQL.Parser.Name.Introspection,
    module Hasura.GraphQL.Parser.Name.TypeSystem,
    builtInScalars,
  )
where

import Data.HashSet (HashSet)
import Data.HashSet qualified as Set
import Hasura.GraphQL.Parser.Name.Introspection
import Hasura.GraphQL.Parser.Name.TypeSystem
import Language.GraphQL.Draft.Syntax qualified as G

-- | A set of the type names of GraphQL's built-in scalars.
builtInScalars :: HashSet G.Name
builtInScalars = Set.fromList [_Boolean, _Float, _ID, _Int, _String]
