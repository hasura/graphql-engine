module Hasura.GraphQL.Parser.DirectivesTest where

import           Hasura.Prelude

import qualified Data.Dependent.Map               as DM
import qualified Data.Text                        as T
import qualified Language.GraphQL.Draft.Syntax    as G

import           Test.Hspec

import           Hasura.GraphQL.Parser.Directives
import           Hasura.GraphQL.Parser.Schema
import           Hasura.GraphQL.Parser.TestUtils


spec :: Spec
spec = do
  testDirective skipDirective skip
  testDirective includeDirective include
  testDirective cachedDirective cached
  testDirective multipleRootFieldsDirective multipleRootFields

testDirective :: Directive TestMonad -> DirectiveKey a -> Spec
testDirective dir key = do
  let name      = diName $ dDefinition dir
      location  = head $ diLocations $ dDefinition dir
      directive = fakeDirective $ dDefinition dir
  describe (T.unpack $ G.unName name) $ do
    it "has the same type in the key and the directive" $
      flip shouldBe (Right True) $ runTest $ do
        dmap <- parseDirectives [dir] location [directive]
        pure $ isJust $ runIdentity <$> DM.lookup key dmap
