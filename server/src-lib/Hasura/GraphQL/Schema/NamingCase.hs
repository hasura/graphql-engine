module Hasura.GraphQL.Schema.NamingCase
  ( isGraphqlCase,
  )
where

import Hasura.Prelude
import Hasura.RQL.Types.NamingCase

isGraphqlCase :: NamingCase -> Bool
isGraphqlCase GraphqlCase = True
isGraphqlCase _ = False
