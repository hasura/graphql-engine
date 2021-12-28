module Hasura.GraphQL.Parser.DirectivesTest (spec) where

import Data.Dependent.Map qualified as DM
import Data.Text qualified as T
import Hasura.GraphQL.Parser.Directives
import Hasura.GraphQL.Parser.Schema
import Hasura.GraphQL.Parser.TestUtils
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec

spec :: Spec
spec = do
  testDirective skipDirective skip
  testDirective includeDirective include
  testDirective cachedDirective cached
  testDirective multipleRootFieldsDirective multipleRootFields

testDirective :: Directive TestMonad -> DirectiveKey a -> Spec
testDirective dir key = do
  let name = diName $ dDefinition dir
      location = head $ diLocations $ dDefinition dir
      directive = fakeDirective $ dDefinition dir
  describe (T.unpack $ G.unName name) $ do
    it "has the same type in the key and the directive" $
      flip shouldBe (Right True) $
        runTest $ do
          dmap <- parseDirectives [dir] location [directive]
          pure $ isJust $ runIdentity <$> DM.lookup key dmap
