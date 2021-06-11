module Hasura.GraphQL.Schema.RemoteTest (spec) where

import           Hasura.Prelude

import qualified Data.Aeson                            as J
import qualified Data.ByteString.Lazy                  as LBS
import qualified Data.HashMap.Strict                   as M
import qualified Data.Text                             as T
import qualified Language.GraphQL.Draft.Parser         as G
import qualified Language.GraphQL.Draft.Syntax         as G
import qualified Network.URI                           as N

import           Data.Text.Extended
import           Data.Text.RawString
import           Test.Hspec

import qualified Hasura.GraphQL.Parser.Internal.Parser as P

import           Hasura.Base.Error
import           Hasura.GraphQL.Execute.Inline
import           Hasura.GraphQL.Execute.Resolve
import           Hasura.GraphQL.Parser.Monad
import           Hasura.GraphQL.Parser.Schema
import           Hasura.GraphQL.Parser.TestUtils
import           Hasura.GraphQL.Schema.Remote
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCache
import           Hasura.Session


-- test tools

runError :: Monad m => ExceptT QErr m a -> m a
runError = runExceptT >=> (`onLeft` (error . T.unpack . qeError))


mkTestRemoteSchema :: Text -> RemoteSchemaIntrospection
mkTestRemoteSchema schema = RemoteSchemaIntrospection $ runIdentity $ runError $ do
  G.SchemaDocument types <- G.parseSchemaDocument schema `onLeft` throw500
  pure $ flip mapMaybe types \case
    G.TypeSystemDefinitionSchema _ -> Nothing
    G.TypeSystemDefinitionType td  -> Just $ case fmap toRemoteInputValue td of
      G.TypeDefinitionScalar      std -> G.TypeDefinitionScalar std
      G.TypeDefinitionObject      otd -> G.TypeDefinitionObject otd
      G.TypeDefinitionUnion       utd -> G.TypeDefinitionUnion  utd
      G.TypeDefinitionEnum        etd -> G.TypeDefinitionEnum   etd
      G.TypeDefinitionInputObject itd -> G.TypeDefinitionInputObject itd
      G.TypeDefinitionInterface   itd -> G.TypeDefinitionInterface $ G.InterfaceTypeDefinition
        { G._itdDescription      = G._itdDescription      itd
        , G._itdName             = G._itdName             itd
        , G._itdDirectives       = G._itdDirectives       itd
        , G._itdFieldsDefinition = G._itdFieldsDefinition itd
        , G._itdPossibleTypes    = []
        }
  where
    toRemoteInputValue ivd = RemoteSchemaInputValueDefinition
      { _rsitdDefinition     = ivd
      , _rsitdPresetArgument = choice $ G._ivdDirectives ivd <&> \dir -> do
          guard $ G._dName dir == $$(G.litName "preset")
          value <- M.lookup $$(G.litName "value") $ G._dArguments dir
          Just $ case value of
            G.VString "x-hasura-test" -> G.VVariable $
              SessionPresetVariable (mkSessionVariable "x-hasura-test") $$(G.litName "String") SessionArgumentPresetScalar
            _ -> absurd <$> value
      }

mkTestExecutableDocument :: Text -> ([G.VariableDefinition], G.SelectionSet G.NoFragments G.Name)
mkTestExecutableDocument t = runIdentity $ runError $ do
  G.ExecutableDocument execDoc <- G.parseExecutableDoc t `onLeft` throw500
  case execDoc of
    [G.ExecutableDefinitionOperation op] -> case op of
      G.OperationDefinitionUnTyped selSet -> ([],) <$> inlineSelectionSet [] selSet
      G.OperationDefinitionTyped   opDef  -> do
        unless (G._todType opDef == G.OperationTypeQuery) $
          throw500 "only queries for now"
        resSelSet <- inlineSelectionSet [] $ G._todSelectionSet opDef
        pure (G._todVariableDefinitions opDef, resSelSet)
    _ -> throw500 "must have only one query in the document"

mkTestVariableValues :: LBS.ByteString -> M.HashMap G.Name J.Value
mkTestVariableValues vars = runIdentity $ runError $ do
  value <- J.eitherDecode vars `onLeft` (throw500 . T.pack)
  case value of
    J.Object vs -> M.fromList <$> for (M.toList vs) \(name, val) -> do
      gname <- G.mkName name `onNothing` throw500 ("wrong Name: " <>> name)
      pure (gname, val)
    _           -> throw500 "variables must be an object"


buildQueryParsers
  :: RemoteSchemaIntrospection
  -> IO (P.FieldParser TestMonad (G.Field G.NoFragments RemoteSchemaVariable))
buildQueryParsers introspection = do
  let introResult = IntrospectionResult introspection $$(G.litName "Query") Nothing Nothing
  (query, _, _) <- runError
    $ runSchemaT
    $ buildRemoteParser introResult
    $ RemoteSchemaInfo
      N.nullURI [] False 60
  pure $ head query <&> \(RemoteFieldG _ f) -> f


runQueryParser
  :: P.FieldParser TestMonad a
  -> ([G.VariableDefinition], G.SelectionSet G.NoFragments G.Name)
  -> M.HashMap G.Name J.Value
  -> a
runQueryParser parser (varDefs, selSet) vars = runIdentity $ runError $ do
  (_, resolvedSelSet) <- resolveVariables varDefs vars [] selSet
  field <- case resolvedSelSet of
    [G.SelectionField f] -> pure f
    _                    -> error "expecting only one field in the query"
  runTest (P.fParser parser field) `onLeft` throw500

run
  :: Text           -- schema
  -> Text           -- query
  -> LBS.ByteString -- variables
  -> IO (G.Field G.NoFragments RemoteSchemaVariable)
run s q v = do
  parser <- buildQueryParsers $ mkTestRemoteSchema s
  pure $ runQueryParser
    parser
    (mkTestExecutableDocument q)
    (mkTestVariableValues v)


-- actual test

spec :: Spec
spec = do
  testNoVarExpansionIfNoPreset
  testNoVarExpansionIfNoPresetUnlessTopLevelOptionalField
  testPartialVarExpansionIfPreset

testNoVarExpansionIfNoPreset :: Spec
testNoVarExpansionIfNoPreset = it "variables aren't expanded if there's no preset" $ do
  field <- run
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
  let arg = head $ M.toList $ G._fArguments field
  arg `shouldBe`
    ( $$(G.litName "a")
    -- the parser did not create a new JSON variable, and forwarded the query variable unmodified
    , G.VVariable $ QueryVariable $ Variable
      (VIRequired $$(G.litName "a"))
      (G.TypeNamed (G.Nullability False) $$(G.litName "A"))
      (JSONValue $ J.Object $ M.fromList [("b", J.Object $ M.fromList [("c", J.Object $ M.fromList [("i", J.Number 0)])])])
    )

testNoVarExpansionIfNoPresetUnlessTopLevelOptionalField :: Spec
testNoVarExpansionIfNoPresetUnlessTopLevelOptionalField = it "unless fieldOptional peels the variable first" $ do
  field <- run
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
  let arg = head $ M.toList $ G._fArguments field
  arg `shouldBe`
    ( $$(G.litName "a")
    -- fieldOptional has peeled the variable; all we see is a JSON blob, and in doubt
    -- we repackage it as a newly minted JSON variable
    , G.VVariable $ RemoteJSONValue
      (G.TypeNamed (G.Nullability True) $$(G.litName "A"))
      (J.Object $ M.fromList [("b", J.Object $ M.fromList [("c", J.Object $ M.fromList [("i", J.Number 0)])])])
    )

testPartialVarExpansionIfPreset :: Spec
testPartialVarExpansionIfPreset = it "presets cause partial var expansion" $ do
  field <- run
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
  let arg = head $ M.toList $ G._fArguments field
  arg `shouldBe`
    ( $$(G.litName "a")
    -- the preset has caused partial variable expansion, only up to where it's needed
    , G.VObject $ M.fromList
      [ ( $$(G.litName "x")
        , G.VInt 0
        )
      , ( $$(G.litName "b")
        , G.VVariable $ RemoteJSONValue
          (G.TypeNamed (G.Nullability True) $$(G.litName "B"))
          (J.Object $ M.fromList [("c", J.Object $ M.fromList [("i", J.Number 0)])])
        )
      ]
    )
