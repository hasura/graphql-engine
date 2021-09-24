{-# LANGUAGE TupleSections #-}

module Hasura.GraphQL.RemoteServerSpec (spec) where

import Data.Containers.ListUtils (nubOrd)
import Data.Either (isRight)
import Data.HashMap.Strict qualified as Map
import Hasura.Generator ()
import Hasura.GraphQL.RemoteServer
import Hasura.Prelude
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.SchemaCache
import Language.GraphQL.Draft.Syntax qualified as G
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

spec :: Spec
spec = do
  describe "IntrospectionResult" $ do
    describe "getCustomizer" $ do
      prop "inverse" $
        forAllShrinkShow gen shrink_ show_ $ \(introspectionResult, typesAndFields, customization) ->
          let customizer = getCustomizer introspectionResult (Just customization)
              customizeTypeName = remoteSchemaCustomizeTypeName customizer
              customizeFieldName = remoteSchemaCustomizeFieldName customizer
              decustomizeTypeName = remoteSchemaDecustomizeTypeName customizer
              decustomizeFieldName = remoteSchemaDecustomizeFieldName customizer
              typeTests =
                conjoin $
                  Map.keys typesAndFields <&> \typeName ->
                    decustomizeTypeName (customizeTypeName typeName) === typeName
              fieldTests =
                conjoin $
                  Map.toList typesAndFields <&> \(typeName, fieldNames) ->
                    conjoin $
                      fieldNames <&> \fieldName ->
                        decustomizeFieldName (customizeTypeName typeName) (customizeFieldName typeName fieldName) === fieldName
           in isRight (validateSchemaCustomizationsDistinct customizer $ irDoc introspectionResult)
                ==> typeTests .&&. fieldTests

getTypesAndFields :: IntrospectionResult -> HashMap G.Name [G.Name]
getTypesAndFields IntrospectionResult {irDoc = RemoteSchemaIntrospection typeDefinitions} =
  Map.fromList $ map getTypeAndFields typeDefinitions
  where
    getTypeAndFields = \case
      G.TypeDefinitionScalar G.ScalarTypeDefinition {..} -> (_stdName, [])
      G.TypeDefinitionObject G.ObjectTypeDefinition {..} -> (_otdName, G._fldName <$> _otdFieldsDefinition)
      G.TypeDefinitionInterface G.InterfaceTypeDefinition {..} -> (_itdName, G._fldName <$> _itdFieldsDefinition)
      G.TypeDefinitionUnion G.UnionTypeDefinition {..} -> (_utdName, [])
      G.TypeDefinitionEnum G.EnumTypeDefinition {..} -> (_etdName, [])
      G.TypeDefinitionInputObject G.InputObjectTypeDefinition {..} -> (_iotdName, [])

genCustomization :: HashMap G.Name [G.Name] -> Gen RemoteSchemaCustomization
genCustomization typesAndFields = RemoteSchemaCustomization <$> arbitrary <*> fmap Just genTypeNames <*> fmap Just genFieldNames
  where
    genTypeNames = RemoteTypeCustomization <$> arbitrary <*> arbitrary <*> genMap (Map.keys typesAndFields)
    genFieldNames = do
      typesAndFields' <- sublistOf $ Map.toList typesAndFields
      for typesAndFields' $ \(typeName, fieldNames) ->
        RemoteFieldCustomization typeName <$> arbitrary <*> arbitrary <*> genMap fieldNames
    genMap names = do
      keys <- sublistOf names
      values <- nubOrd . filter (`notElem` names) <$> infiniteList
      pure $ Map.fromList $ zip keys values

gen :: Gen (IntrospectionResult, HashMap G.Name [G.Name], RemoteSchemaCustomization)
gen = do
  introspectionResult <- arbitrary
  let typesAndFields = getTypesAndFields introspectionResult
  customization <- genCustomization typesAndFields
  pure (introspectionResult, typesAndFields, customization)

shrink_ :: (IntrospectionResult, HashMap G.Name [G.Name], RemoteSchemaCustomization) -> [(IntrospectionResult, HashMap G.Name [G.Name], RemoteSchemaCustomization)]
shrink_ (introspectionResult, typesAndFields, customization@RemoteSchemaCustomization {..}) =
  (shrinkCustomization <&> (introspectionResult,typesAndFields,))
    ++ (shrinkTypesAndFields <&> (introspectionResult,,customization))
  where
    shrinkCustomization = shrinkNamespace ++ shrinkTypeNames ++ shrinkFieldNames

    shrinkMaybe _ Nothing = []
    shrinkMaybe f (Just x) = Nothing : (Just <$> f x)

    shrinkMaybe' = shrinkMaybe shrinkNothing

    shrinkHashMap f = shrinkMapBy Map.fromList Map.toList $ shrinkList f

    shrinkHashMap' = shrinkHashMap shrinkNothing

    shrinkNamespace = do
      ns <- shrinkMaybe' _rscRootFieldsNamespace
      pure $ customization {_rscRootFieldsNamespace = ns}

    shrinkTypeNames = do
      tns <- shrinkMaybe shrinkTypeNames' _rscTypeNames
      pure $ customization {_rscTypeNames = tns}

    shrinkTypeNames' rtc@RemoteTypeCustomization {..} =
      (shrinkMaybe' _rtcPrefix <&> \p -> rtc {_rtcPrefix = p})
        ++ (shrinkMaybe' _rtcSuffix <&> \s -> rtc {_rtcSuffix = s})
        ++ (shrinkHashMap' _rtcMapping <&> \m -> rtc {_rtcMapping = m})

    shrinkFieldNames = do
      fns <- shrinkMaybe (shrinkList shrinkFieldNames') _rscFieldNames
      pure $ customization {_rscFieldNames = fns}

    shrinkFieldNames' rfc@RemoteFieldCustomization {..} =
      (shrinkMaybe' _rfcPrefix <&> \p -> rfc {_rfcPrefix = p})
        ++ (shrinkMaybe' _rfcSuffix <&> \s -> rfc {_rfcSuffix = s})
        ++ (shrinkHashMap' _rfcMapping <&> \m -> rfc {_rfcMapping = m})

    shrinkTypesAndFields = shrinkHashMap (traverse $ shrinkList shrinkNothing) typesAndFields

show_ :: (IntrospectionResult, HashMap G.Name [G.Name], RemoteSchemaCustomization) -> String
show_ (_a, b, c) = show (b, c)
