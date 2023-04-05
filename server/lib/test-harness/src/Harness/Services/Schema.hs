module Harness.Services.Schema (withSchemaName) where

import Harness.Schema.Name
import Test.Hspec

withSchemaName :: SchemaName -> SpecWith (SchemaName, testEnvironment) -> SpecWith testEnvironment
withSchemaName schemaName = aroundWith (\action testEnvironment -> action (schemaName, testEnvironment))
