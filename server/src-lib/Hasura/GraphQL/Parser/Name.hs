-- | Re-exports everything from the sub-modules.
--
-- Submodules contain names relevant to the GraphQL specification, October 2021.
-- https://spec.graphql.org/October2021/
--
-- Each name is placed in a Haddock section corresponding to its section in the specification.
module Hasura.GraphQL.Parser.Name
  ( module Hasura.GraphQL.Parser.Name.Introspection,
    module Hasura.GraphQL.Parser.Name.TypeSystem,
  )
where

import Hasura.GraphQL.Parser.Name.Introspection
import Hasura.GraphQL.Parser.Name.TypeSystem
