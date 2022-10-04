module Hasura.GraphQL.Parser.Schema.Convert
  ( convertType,
  )
where

import {-# SOURCE #-} Hasura.GraphQL.Parser.Schema
import Language.GraphQL.Draft.Syntax qualified as G

convertType :: SomeDefinitionTypeInfo origin -> G.TypeDefinition [G.Name] G.InputValueDefinition
