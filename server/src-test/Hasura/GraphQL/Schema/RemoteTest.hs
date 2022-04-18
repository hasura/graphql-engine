{-# LANGUAGE QuasiQuotes #-}

module Hasura.GraphQL.Schema.RemoteTest (spec) where

import Control.Lens (Prism', prism', to, (^..), _Right)
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.HashMap.Strict.Extended qualified as M
import Data.Text qualified as T
import Data.Text.Extended
import Data.Text.RawString
import Hasura.Base.Error
import Hasura.GraphQL.Execute.Inline
import Hasura.GraphQL.Execute.Remote (resolveRemoteVariable, runVariableCache)
import Hasura.GraphQL.Execute.Resolve
import Hasura.GraphQL.Namespace
import Hasura.GraphQL.Parser.Column (UnpreparedValue)
import Hasura.GraphQL.Parser.Constants qualified as G
import Hasura.GraphQL.Parser.Internal.Parser qualified as P
import Hasura.GraphQL.Parser.Monad
import Hasura.GraphQL.Parser.Schema
import Hasura.GraphQL.Parser.TestUtils
import Hasura.GraphQL.Schema.Common
import Hasura.GraphQL.Schema.Remote
import Hasura.Prelude
import Hasura.RQL.IR.RemoteSchema
import Hasura.RQL.IR.Root
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SchemaCache
import Hasura.RQL.Types.Source
import Hasura.RQL.Types.SourceCustomization
import Hasura.Session
import Language.GraphQL.Draft.Parser qualified as G
import Language.GraphQL.Draft.Syntax qualified as G
import Network.URI qualified as N
import Test.Hspec

-- test tools

runError :: Monad m => ExceptT QErr m a -> m a
runError = runExceptT >=> (`onLeft` (error . T.unpack . qeError))

mkTestRemoteSchema :: Text -> RemoteSchemaIntrospection
mkTestRemoteSchema schema = RemoteSchemaIntrospection $
  M.fromListOn getTypeName $
    runIdentity $
      runError $ do
        G.SchemaDocument types <- G.parseSchemaDocument schema `onLeft` throw500
        pure $ flip mapMaybe types \case
          G.TypeSystemDefinitionSchema _ -> Nothing
          G.TypeSystemDefinitionType td -> Just $ case fmap toRemoteInputValue td of
            G.TypeDefinitionScalar std -> G.TypeDefinitionScalar std
            G.TypeDefinitionObject otd -> G.TypeDefinitionObject otd
            G.TypeDefinitionUnion utd -> G.TypeDefinitionUnion utd
            G.TypeDefinitionEnum etd -> G.TypeDefinitionEnum etd
            G.TypeDefinitionInputObject itd -> G.TypeDefinitionInputObject itd
            G.TypeDefinitionInterface itd ->
              G.TypeDefinitionInterface $
                G.InterfaceTypeDefinition
                  { G._itdDescription = G._itdDescription itd,
                    G._itdName = G._itdName itd,
                    G._itdDirectives = G._itdDirectives itd,
                    G._itdFieldsDefinition = G._itdFieldsDefinition itd,
                    G._itdPossibleTypes = []
                  }
  where
    toRemoteInputValue ivd =
      RemoteSchemaInputValueDefinition
        { _rsitdDefinition = ivd,
          _rsitdPresetArgument =
            choice $
              G._ivdDirectives ivd <&> \dir -> do
                guard $ G._dName dir == G._preset
                value <- M.lookup G._value $ G._dArguments dir
                Just $ case value of
                  G.VString "x-hasura-test" ->
                    G.VVariable $
                      SessionPresetVariable (mkSessionVariable "x-hasura-test") G._String SessionArgumentPresetScalar
                  _ -> absurd <$> value
        }

mkTestExecutableDocument :: Text -> ([G.VariableDefinition], G.SelectionSet G.NoFragments G.Name)
mkTestExecutableDocument t = runIdentity $
  runError $ do
    G.ExecutableDocument execDoc <- G.parseExecutableDoc t `onLeft` throw500
    case execDoc of
      [G.ExecutableDefinitionOperation op] -> case op of
        G.OperationDefinitionUnTyped selSet -> ([],) <$> inlineSelectionSet [] selSet
        G.OperationDefinitionTyped opDef -> do
          unless (G._todType opDef == G.OperationTypeQuery) $
            throw500 "only queries for now"
          resSelSet <- inlineSelectionSet [] $ G._todSelectionSet opDef
          pure (G._todVariableDefinitions opDef, resSelSet)
      _ -> throw500 "must have only one query in the document"

mkTestVariableValues :: LBS.ByteString -> M.HashMap G.Name J.Value
mkTestVariableValues vars = runIdentity $
  runError $ do
    value <- J.eitherDecode vars `onLeft` (throw500 . T.pack)
    case value of
      J.Object vs ->
        M.fromList <$> for (M.toList vs) \(name, val) -> do
          gname <- G.mkName name `onNothing` throw500 ("wrong Name: " <>> name)
          pure (gname, val)
      _ -> throw500 "variables must be an object"

buildQueryParsers ::
  RemoteSchemaIntrospection ->
  IO (P.FieldParser TestMonad (GraphQLField (RemoteRelationshipField UnpreparedValue) RemoteSchemaVariable))
buildQueryParsers introspection = do
  let introResult = IntrospectionResult introspection G._Query Nothing Nothing
      remoteSchemaInfo = RemoteSchemaInfo (ValidatedRemoteSchemaDef N.nullURI [] False 60 Nothing) identityCustomizer
      remoteSchemaRels = mempty
      -- Since remote schemas can theoretically join against tables, we need to
      -- have access to all relevant sources-specific information to build their
      -- schema. Here, since there are no relationships to a source in this
      -- test, we are free to give 'undefined' for such fields, as they won't be
      -- evaluated.
      sourceContext =
        ( adminRoleName :: RoleName,
          undefined :: SourceCache,
          undefined :: QueryContext,
          mempty :: CustomizeRemoteFieldName,
          mempty :: RemoteSchemaMap,
          mempty :: MkTypename,
          mempty :: MkRootFieldName
        )
  RemoteSchemaParser query _ _ <-
    runError $
      flip runReaderT sourceContext $
        runSchemaT $
          buildRemoteParser introResult remoteSchemaRels remoteSchemaInfo
  pure $
    head query <&> \case
      NotNamespaced remoteFld -> _rfField remoteFld
      Namespaced _ ->
        -- Shouldn't happen if we're using identityCustomizer
        -- TODO: add some tests for remote schema customization
        error "buildQueryParsers: unexpected Namespaced field"

runQueryParser ::
  P.FieldParser TestMonad any ->
  ([G.VariableDefinition], G.SelectionSet G.NoFragments G.Name) ->
  M.HashMap G.Name J.Value ->
  any
runQueryParser parser (varDefs, selSet) vars = runIdentity . runError $ do
  (_, resolvedSelSet) <- resolveVariables varDefs vars [] selSet
  field <- case resolvedSelSet of
    [G.SelectionField f] -> pure f
    _ -> error "expecting only one field in the query"
  runTest (P.fParser parser field) `onLeft` throw500

run ::
  -- | schema
  Text ->
  -- | query
  Text ->
  -- | variables
  LBS.ByteString ->
  IO (GraphQLField (RemoteRelationshipField UnpreparedValue) RemoteSchemaVariable)
run schema query variables = do
  parser <- buildQueryParsers $ mkTestRemoteSchema schema
  pure $
    runQueryParser
      parser
      (mkTestExecutableDocument query)
      (mkTestVariableValues variables)

-- actual test

spec :: Spec
spec = do
  testNoVarExpansionIfNoPreset
  testNoVarExpansionIfNoPresetUnlessTopLevelOptionalField
  testPartialVarExpansionIfPreset
  testVariableSubstitutionCollision

testNoVarExpansionIfNoPreset :: Spec
testNoVarExpansionIfNoPreset = it "variables aren't expanded if there's no preset" $ do
  field <-
    run
      -- schema
      [raw|
scalar Int

input A {
  b: B
}

input B {
  c: C
}

input C {
  i: Int
}

type Query {
  test(a: A!): Int
}
|]
      -- query
      [raw|
query($a: A!) {
  test(a: $a)
}
|]
      -- variables
      [raw|
{
  "a": {
    "b": {
      "c": {
        "i": 0
      }
    }
  }
}
|]
  let arg = head $ M.toList $ _fArguments field
  arg
    `shouldBe` ( G._a,
                 -- the parser did not create a new JSON variable, and forwarded the query variable unmodified
                 G.VVariable $
                   QueryVariable $
                     Variable
                       (VIRequired G._a)
                       (G.TypeNamed (G.Nullability False) G._A)
                       (JSONValue $ J.Object $ M.fromList [("b", J.Object $ M.fromList [("c", J.Object $ M.fromList [("i", J.Number 0)])])])
               )

testNoVarExpansionIfNoPresetUnlessTopLevelOptionalField :: Spec
testNoVarExpansionIfNoPresetUnlessTopLevelOptionalField = it "unless fieldOptional peels the variable first" $ do
  field <-
    run
      -- schema
      [raw|
scalar Int

input A {
  b: B
}

input B {
  c: C
}

input C {
  i: Int
}

type Query {
  test(a: A): Int
}
|]
      -- query
      [raw|
query($a: A) {
  test(a: $a)
}
|]
      -- variables
      [raw|
{
  "a": {
    "b": {
      "c": {
        "i": 0
      }
    }
  }
}
|]
  let arg = head $ M.toList $ _fArguments field
  arg
    `shouldBe` ( G._a,
                 -- fieldOptional has peeled the variable; all we see is a JSON blob, and in doubt
                 -- we repackage it as a newly minted JSON variable
                 G.VVariable $
                   RemoteJSONValue
                     (G.TypeNamed (G.Nullability True) G._A)
                     (J.Object $ M.fromList [("b", J.Object $ M.fromList [("c", J.Object $ M.fromList [("i", J.Number 0)])])])
               )

testPartialVarExpansionIfPreset :: Spec
testPartialVarExpansionIfPreset = it "presets cause partial var expansion" $ do
  field <-
    run
      -- schema
      [raw|
scalar Int

input A {
  x: Int @preset(value: 0)
  b: B
}

input B {
  c: C
}

input C {
  i: Int
}

type Query {
  test(a: A!): Int
}
|]
      -- query
      [raw|
query($a: A!) {
  test(a: $a)
}
|]
      -- variables
      [raw|
{
  "a": {
    "b": {
      "c": {
        "i": 0
      }
    }
  }
}
|]
  let arg = head $ M.toList $ _fArguments field
  arg
    `shouldBe` ( G._a,
                 -- the preset has caused partial variable expansion, only up to where it's needed
                 G.VObject $
                   M.fromList
                     [ ( G._x,
                         G.VInt 0
                       ),
                       ( G._b,
                         G.VVariable $
                           RemoteJSONValue
                             (G.TypeNamed (G.Nullability True) G._B)
                             (J.Object $ M.fromList [("c", J.Object $ M.fromList [("i", J.Number 0)])])
                       )
                     ]
               )

-- | Regression test for https://github.com/hasura/graphql-engine/issues/7170
testVariableSubstitutionCollision :: Spec
testVariableSubstitutionCollision = it "ensures that remote variables are de-duplicated by type and value, not just by value" $ do
  field <- run schema query variables
  let dummyUserInfo =
        UserInfo
          adminRoleName
          (mempty @SessionVariables)
          BOFADisallowed
  eField <-
    runExceptT
      . runVariableCache
      . traverse (resolveRemoteVariable dummyUserInfo)
      $ field
  let variableNames =
        eField ^.. _Right . to _fArguments . traverse . _VVariable . to vInfo . to getName . to G.unName
  variableNames `shouldBe` ["hasura_json_var_1", "hasura_json_var_2"]
  where
    -- A schema whose values are representable as collections of JSON values.
    schema :: Text
    schema =
      [raw|
scalar Int
scalar String

type Query {
  test(a: [Int], b: [String]): Int
}
|]
    -- A query against values from 'schema' using JSON variable substitution.
    query :: Text
    query =
      [raw|
query($a: [Int], $b: [String]) {
  test(a: $a, b: $b)
}
|]
    -- Two identical JSON variables to substitute; 'schema' and 'query' declare
    -- that these variables should have different types despite both being
    -- empty collections.
    variables :: LBS.ByteString
    variables =
      [raw|
{
  "a": [],
  "b": []
}
|]

-- | Convenience function to focus on a 'G.VVariable' when pulling test values
-- out in 'testVariableSubstitutionCollision'.
_VVariable :: Prism' (G.Value var) var
_VVariable = prism' upcast downcast
  where
    upcast = G.VVariable
    downcast = \case
      G.VVariable var -> Just var
      _ -> Nothing
