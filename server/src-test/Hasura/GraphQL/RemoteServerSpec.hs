{-# LANGUAGE TupleSections #-}
module Hasura.GraphQL.RemoteServerSpec (spec) where

import           Hasura.Prelude

import qualified Data.Aeson                    as J
import qualified Data.Aeson.Types              as J
import qualified Data.HashMap.Strict           as Map
import qualified Language.GraphQL.Draft.Syntax as G

import           Data.ByteString.Lazy          (ByteString)
import           Data.Containers.ListUtils     (nubOrd)
import           Data.Either                   (isRight)
import           Data.Text.RawString           (raw)
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Hasura.Generator              ()
import           Hasura.GraphQL.RemoteServer
import           Hasura.RQL.Types.RemoteSchema
import           Hasura.RQL.Types.SchemaCache


spec :: Spec
spec = do
  describe "IntrospectionResult" $ do
    prop "JSON roundtrip" $ forAll (scale (`div` 10) arbitrary) $ -- Use scale to ensure tests run in a reasonable time
      \introspectionResult -> do
        let json = introspectionResultToJSON introspectionResult
        J.parse parseIntrospectionResult json `shouldBe` J.Success introspectionResult
        customizeIntrospectionResult identityCustomizer introspectionResult `shouldBe` introspectionResult

    it "Example roundtrip" $
      case J.decode rawIntrospectionResult of
        Nothing -> error "Failed to decode rawIntrospectionResult"
        Just json ->
          case J.parse parseIntrospectionResult json of
            J.Success introspectionResult -> do
              let customizedIntrospectionResult = customizeIntrospectionResult identityCustomizer introspectionResult
              customizedIntrospectionResult `shouldBe` introspectionResult
              introspectionResultToJSON customizedIntrospectionResult `shouldBe` json
            _ -> error "Failed to parse rawIntrospectionResult"

    describe "getCustomizer" $ do
      prop "inverse" $
        forAllShrinkShow gen shrink_ show_ $ \(introspectionResult, typesAndFields, customization) ->
          let customizer = getCustomizer introspectionResult (Just customization)
              customizeTypeName = remoteSchemaCustomizeTypeName customizer
              customizeFieldName = remoteSchemaCustomizeFieldName customizer
              decustomizeTypeName = remoteSchemaDecustomizeTypeName customizer
              decustomizeFieldName = remoteSchemaDecustomizeFieldName customizer
              typeTests = conjoin $ Map.keys typesAndFields <&> \typeName ->
                decustomizeTypeName (customizeTypeName typeName) === typeName
              fieldTests = conjoin $ Map.toList typesAndFields <&> \(typeName, fieldNames) ->
                conjoin $ fieldNames <&> \fieldName ->
                  decustomizeFieldName (customizeTypeName typeName) (customizeFieldName typeName fieldName) === fieldName
          in
            isRight (validateSchemaCustomizationsDistinct customizer $ irDoc introspectionResult) ==>
              typeTests .&&. fieldTests

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
  (shrinkCustomization <&> (introspectionResult, typesAndFields,)) ++
  (shrinkTypesAndFields <&> (introspectionResult,,customization))
  where
    shrinkCustomization = shrinkNamespace ++ shrinkTypeNames ++ shrinkFieldNames

    shrinkMaybe _ Nothing  = []
    shrinkMaybe f (Just x) = Nothing : (Just <$> f x)

    shrinkMaybe' = shrinkMaybe shrinkNothing

    shrinkHashMap f = shrinkMapBy Map.fromList Map.toList $ shrinkList f

    shrinkHashMap' = shrinkHashMap shrinkNothing

    shrinkNamespace = do
      ns <- shrinkMaybe' _rscRootFieldsNamespace
      pure $ customization { _rscRootFieldsNamespace = ns }

    shrinkTypeNames = do
      tns <- shrinkMaybe shrinkTypeNames' _rscTypeNames
      pure $ customization { _rscTypeNames = tns}

    shrinkTypeNames' rtc@RemoteTypeCustomization{..} =
      (shrinkMaybe' _rtcPrefix <&> \p -> rtc { _rtcPrefix = p}) ++
      (shrinkMaybe' _rtcSuffix <&> \s -> rtc { _rtcSuffix = s}) ++
      (shrinkHashMap' _rtcMapping <&> \m -> rtc { _rtcMapping = m})

    shrinkFieldNames = do
      fns <- shrinkMaybe (shrinkList shrinkFieldNames') _rscFieldNames
      pure $ customization { _rscFieldNames = fns }

    shrinkFieldNames' rfc@RemoteFieldCustomization{..} =
      (shrinkMaybe' _rfcPrefix <&> \p -> rfc { _rfcPrefix = p}) ++
      (shrinkMaybe' _rfcSuffix <&> \s -> rfc { _rfcSuffix = s}) ++
      (shrinkHashMap' _rfcMapping <&> \m -> rfc { _rfcMapping = m})

    shrinkTypesAndFields = shrinkHashMap (traverse $ shrinkList shrinkNothing) typesAndFields

show_ :: (IntrospectionResult, HashMap G.Name [G.Name], RemoteSchemaCustomization) -> String
show_ (_a, b, c) = show (b, c)

rawIntrospectionResult :: ByteString
rawIntrospectionResult = [raw|
{
  "data": {
    "__schema": {
      "queryType": {
        "name": "query_root"
      },
      "subscriptionType": {
        "name": "subscription_root"
      },
      "types": [
        {
          "kind": "SCALAR",
          "name": "Boolean"
        },
        {
          "kind": "SCALAR",
          "name": "Float"
        },
        {
          "kind": "SCALAR",
          "name": "ID"
        },
        {
          "kind": "SCALAR",
          "name": "Int"
        },
        {
          "kind": "SCALAR",
          "name": "String"
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "__Directive",
          "fields": [
            {
              "args": [],
              "name": "args",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__InputValue"
                    }
                  }
                }
              }
            },
            {
              "args": [],
              "name": "description",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [],
              "name": "locations",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "ENUM",
                      "name": "__DirectiveLocation"
                    }
                  }
                }
              }
            },
            {
              "args": [],
              "name": "name",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "String"
                }
              }
            }
          ]
        },
        {
          "kind": "ENUM",
          "name": "__DirectiveLocation",
          "enumValues": [
            {
              "name": "ARGUMENT_DEFINITION"
            },
            {
              "name": "ENUM"
            },
            {
              "name": "ENUM_VALUE"
            },
            {
              "name": "FIELD"
            },
            {
              "name": "FIELD_DEFINITION"
            },
            {
              "name": "FRAGMENT_DEFINITION"
            },
            {
              "name": "FRAGMENT_SPREAD"
            },
            {
              "name": "INLINE_FRAGMENT"
            },
            {
              "name": "INPUT_FIELD_DEFINITION"
            },
            {
              "name": "INPUT_OBJECT"
            },
            {
              "name": "INTERFACE"
            },
            {
              "name": "MUTATION"
            },
            {
              "name": "OBJECT"
            },
            {
              "name": "QUERY"
            },
            {
              "name": "SCALAR"
            },
            {
              "name": "SCHEMA"
            },
            {
              "name": "SUBSCRIPTION"
            },
            {
              "name": "UNION"
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "__EnumValue",
          "fields": [
            {
              "args": [],
              "name": "deprecationReason",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [],
              "name": "description",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [],
              "name": "isDeprecated",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "Boolean"
                }
              }
            },
            {
              "args": [],
              "name": "name",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "String"
                }
              }
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "__Field",
          "fields": [
            {
              "args": [],
              "name": "args",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__InputValue"
                    }
                  }
                }
              }
            },
            {
              "args": [],
              "name": "deprecationReason",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [],
              "name": "description",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [],
              "name": "isDeprecated",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "Boolean"
                }
              }
            },
            {
              "args": [],
              "name": "name",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "String"
                }
              }
            },
            {
              "args": [],
              "name": "type",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "OBJECT",
                  "name": "__Type"
                }
              }
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "__InputValue",
          "fields": [
            {
              "args": [],
              "name": "defaultValue",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [],
              "name": "description",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [],
              "name": "name",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "String"
                }
              }
            },
            {
              "args": [],
              "name": "type",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "OBJECT",
                  "name": "__Type"
                }
              }
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "__Schema",
          "fields": [
            {
              "args": [],
              "name": "directives",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__Directive"
                    }
                  }
                }
              }
            },
            {
              "args": [],
              "name": "mutationType",
              "type": {
                "kind": "OBJECT",
                "name": "__Type"
              }
            },
            {
              "args": [],
              "name": "queryType",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "OBJECT",
                  "name": "__Type"
                }
              }
            },
            {
              "args": [],
              "name": "subscriptionType",
              "type": {
                "kind": "OBJECT",
                "name": "__Type"
              }
            },
            {
              "args": [],
              "name": "types",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "__Type"
                    }
                  }
                }
              }
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "__Type",
          "fields": [
            {
              "args": [],
              "name": "description",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [
                {
                  "name": "includeDeprecated",
                  "defaultValue": "false",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Boolean"
                  }
                }
              ],
              "name": "enumValues",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "NON_NULL",
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "__EnumValue"
                  }
                }
              }
            },
            {
              "args": [
                {
                  "name": "includeDeprecated",
                  "defaultValue": "false",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Boolean"
                  }
                }
              ],
              "name": "fields",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "NON_NULL",
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "__Field"
                  }
                }
              }
            },
            {
              "args": [],
              "name": "inputFields",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "NON_NULL",
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "__InputValue"
                  }
                }
              }
            },
            {
              "args": [],
              "name": "interfaces",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "NON_NULL",
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "__Type"
                  }
                }
              }
            },
            {
              "args": [],
              "name": "kind",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "ENUM",
                  "name": "__TypeKind"
                }
              }
            },
            {
              "args": [],
              "name": "name",
              "type": {
                "kind": "SCALAR",
                "name": "String"
              }
            },
            {
              "args": [],
              "name": "ofType",
              "type": {
                "kind": "OBJECT",
                "name": "__Type"
              }
            },
            {
              "args": [],
              "name": "possibleTypes",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "NON_NULL",
                  "ofType": {
                    "kind": "OBJECT",
                    "name": "__Type"
                  }
                }
              }
            }
          ]
        },
        {
          "kind": "ENUM",
          "name": "__TypeKind",
          "enumValues": [
            {
              "name": "ENUM"
            },
            {
              "name": "INPUT_OBJECT"
            },
            {
              "name": "INTERFACE"
            },
            {
              "name": "LIST"
            },
            {
              "name": "NON_NULL"
            },
            {
              "name": "OBJECT"
            },
            {
              "name": "SCALAR"
            },
            {
              "name": "UNION"
            }
          ]
        },
        {
          "kind": "ENUM",
          "name": "conflict_action",
          "enumValues": [
            {
              "name": "ignore",
              "description": "ignore the insert on this row"
            },
            {
              "name": "update",
              "description": "update the row with the given values"
            }
          ],
          "description": "conflict action"
        },
        {
          "inputFields": [
            {
              "name": "_eq",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            },
            {
              "name": "_gt",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            },
            {
              "name": "_gte",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            },
            {
              "name": "_in",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "Int"
                }
              }
            },
            {
              "name": "_is_null",
              "type": {
                "kind": "SCALAR",
                "name": "Boolean"
              }
            },
            {
              "name": "_lt",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            },
            {
              "name": "_lte",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            },
            {
              "name": "_neq",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            },
            {
              "name": "_nin",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "Int"
                }
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "integer_comparison_exp",
          "description": "expression to compare columns of type integer. All fields are combined with logical 'AND'."
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "mutation_root",
          "description": "mutation root",
          "fields": [
            {
              "args": [
                {
                  "name": "where",
                  "type": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "INPUT_OBJECT",
                      "name": "test_bool_exp"
                    }
                  },
                  "description": "filter the rows which have to be deleted"
                }
              ],
              "name": "delete_test",
              "type": {
                "kind": "OBJECT",
                "name": "test_mutation_response"
              },
              "description": "delete data from the table: \"test\""
            },
            {
              "args": [
                {
                  "name": "objects",
                  "type": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "LIST",
                      "ofType": {
                        "kind": "NON_NULL",
                        "ofType": {
                          "kind": "INPUT_OBJECT",
                          "name": "test_insert_input"
                        }
                      }
                    }
                  },
                  "description": "the rows to be inserted"
                },
                {
                  "name": "on_conflict",
                  "type": {
                    "kind": "INPUT_OBJECT",
                    "name": "test_on_conflict"
                  },
                  "description": "on conflict condition"
                }
              ],
              "name": "insert_test",
              "type": {
                "kind": "OBJECT",
                "name": "test_mutation_response"
              },
              "description": "insert data into the table: \"test\""
            },
            {
              "args": [
                {
                  "name": "_inc",
                  "type": {
                    "kind": "INPUT_OBJECT",
                    "name": "test_inc_input"
                  },
                  "description": "increments the integer columns with given value of the filtered values"
                },
                {
                  "name": "_set",
                  "type": {
                    "kind": "INPUT_OBJECT",
                    "name": "test_set_input"
                  },
                  "description": "sets the columns of the filtered rows to the given values"
                },
                {
                  "name": "where",
                  "type": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "INPUT_OBJECT",
                      "name": "test_bool_exp"
                    }
                  },
                  "description": "filter the rows which have to be updated"
                }
              ],
              "name": "update_test",
              "type": {
                "kind": "OBJECT",
                "name": "test_mutation_response"
              },
              "description": "update data of the table: \"test\""
            }
          ]
        },
        {
          "kind": "ENUM",
          "name": "order_by",
          "enumValues": [
            {
              "name": "asc",
              "description": "in the ascending order, nulls last"
            },
            {
              "name": "asc_nulls_first",
              "description": "in the ascending order, nulls first"
            },
            {
              "name": "asc_nulls_last",
              "description": "in the ascending order, nulls last"
            },
            {
              "name": "desc",
              "description": "in the descending order, nulls first"
            },
            {
              "name": "desc_nulls_first",
              "description": "in the descending order, nulls first"
            },
            {
              "name": "desc_nulls_last",
              "description": "in the descending order, nulls last"
            }
          ],
          "description": "column ordering options"
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "query_root",
          "description": "query root",
          "fields": [
            {
              "args": [
                {
                  "name": "distinct_on",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "ENUM",
                        "name": "test_select_column"
                      }
                    }
                  },
                  "description": "distinct select on columns"
                },
                {
                  "name": "limit",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Int"
                  },
                  "description": "limit the nuber of rows returned"
                },
                {
                  "name": "offset",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Int"
                  },
                  "description": "skip the first n rows. Use only with order_by"
                },
                {
                  "name": "order_by",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "INPUT_OBJECT",
                        "name": "test_order_by"
                      }
                    }
                  },
                  "description": "sort the rows by one or more columns"
                },
                {
                  "name": "where",
                  "type": {
                    "kind": "INPUT_OBJECT",
                    "name": "test_bool_exp"
                  },
                  "description": "filter the rows returned"
                }
              ],
              "name": "test",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "test"
                    }
                  }
                }
              },
              "description": "fetch data from the table: \"test\""
            },
            {
              "args": [
                {
                  "name": "distinct_on",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "ENUM",
                        "name": "test_select_column"
                      }
                    }
                  },
                  "description": "distinct select on columns"
                },
                {
                  "name": "limit",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Int"
                  },
                  "description": "limit the nuber of rows returned"
                },
                {
                  "name": "offset",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Int"
                  },
                  "description": "skip the first n rows. Use only with order_by"
                },
                {
                  "name": "order_by",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "INPUT_OBJECT",
                        "name": "test_order_by"
                      }
                    }
                  },
                  "description": "sort the rows by one or more columns"
                },
                {
                  "name": "where",
                  "type": {
                    "kind": "INPUT_OBJECT",
                    "name": "test_bool_exp"
                  },
                  "description": "filter the rows returned"
                }
              ],
              "name": "test_aggregate",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "OBJECT",
                  "name": "test_aggregate"
                }
              },
              "description": "fetch aggregated fields from the table: \"test\""
            },
            {
              "args": [
                {
                  "name": "id",
                  "type": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "SCALAR",
                      "name": "Int"
                    }
                  }
                }
              ],
              "name": "test_by_pk",
              "type": {
                "kind": "OBJECT",
                "name": "test"
              },
              "description": "fetch data from the table: \"test\" using primary key columns"
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "subscription_root",
          "description": "subscription root",
          "fields": [
            {
              "args": [
                {
                  "name": "distinct_on",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "ENUM",
                        "name": "test_select_column"
                      }
                    }
                  },
                  "description": "distinct select on columns"
                },
                {
                  "name": "limit",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Int"
                  },
                  "description": "limit the nuber of rows returned"
                },
                {
                  "name": "offset",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Int"
                  },
                  "description": "skip the first n rows. Use only with order_by"
                },
                {
                  "name": "order_by",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "INPUT_OBJECT",
                        "name": "test_order_by"
                      }
                    }
                  },
                  "description": "sort the rows by one or more columns"
                },
                {
                  "name": "where",
                  "type": {
                    "kind": "INPUT_OBJECT",
                    "name": "test_bool_exp"
                  },
                  "description": "filter the rows returned"
                }
              ],
              "name": "test",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "test"
                    }
                  }
                }
              },
              "description": "fetch data from the table: \"test\""
            },
            {
              "args": [
                {
                  "name": "distinct_on",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "ENUM",
                        "name": "test_select_column"
                      }
                    }
                  },
                  "description": "distinct select on columns"
                },
                {
                  "name": "limit",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Int"
                  },
                  "description": "limit the nuber of rows returned"
                },
                {
                  "name": "offset",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Int"
                  },
                  "description": "skip the first n rows. Use only with order_by"
                },
                {
                  "name": "order_by",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "INPUT_OBJECT",
                        "name": "test_order_by"
                      }
                    }
                  },
                  "description": "sort the rows by one or more columns"
                },
                {
                  "name": "where",
                  "type": {
                    "kind": "INPUT_OBJECT",
                    "name": "test_bool_exp"
                  },
                  "description": "filter the rows returned"
                }
              ],
              "name": "test_aggregate",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "OBJECT",
                  "name": "test_aggregate"
                }
              },
              "description": "fetch aggregated fields from the table: \"test\""
            },
            {
              "args": [
                {
                  "name": "id",
                  "type": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "SCALAR",
                      "name": "Int"
                    }
                  }
                }
              ],
              "name": "test_by_pk",
              "type": {
                "kind": "OBJECT",
                "name": "test"
              },
              "description": "fetch data from the table: \"test\" using primary key columns"
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test",
          "description": "columns and relationships of \"test\"",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "Int"
                }
              }
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_aggregate",
          "description": "aggregated selection of \"test\"",
          "fields": [
            {
              "args": [],
              "name": "aggregate",
              "type": {
                "kind": "OBJECT",
                "name": "test_aggregate_fields"
              }
            },
            {
              "args": [],
              "name": "nodes",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "test"
                    }
                  }
                }
              }
            }
          ]
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_aggregate_fields",
          "description": "aggregate fields of \"test\"",
          "fields": [
            {
              "args": [],
              "name": "avg",
              "type": {
                "kind": "OBJECT",
                "name": "test_avg_fields"
              }
            },
            {
              "args": [
                {
                  "name": "columns",
                  "type": {
                    "kind": "LIST",
                    "ofType": {
                      "kind": "NON_NULL",
                      "ofType": {
                        "kind": "ENUM",
                        "name": "test_select_column"
                      }
                    }
                  }
                },
                {
                  "name": "distinct",
                  "type": {
                    "kind": "SCALAR",
                    "name": "Boolean"
                  }
                }
              ],
              "name": "count",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            },
            {
              "args": [],
              "name": "max",
              "type": {
                "kind": "OBJECT",
                "name": "test_max_fields"
              }
            },
            {
              "args": [],
              "name": "min",
              "type": {
                "kind": "OBJECT",
                "name": "test_min_fields"
              }
            },
            {
              "args": [],
              "name": "stddev",
              "type": {
                "kind": "OBJECT",
                "name": "test_stddev_fields"
              }
            },
            {
              "args": [],
              "name": "stddev_pop",
              "type": {
                "kind": "OBJECT",
                "name": "test_stddev_pop_fields"
              }
            },
            {
              "args": [],
              "name": "stddev_samp",
              "type": {
                "kind": "OBJECT",
                "name": "test_stddev_samp_fields"
              }
            },
            {
              "args": [],
              "name": "sum",
              "type": {
                "kind": "OBJECT",
                "name": "test_sum_fields"
              }
            },
            {
              "args": [],
              "name": "var_pop",
              "type": {
                "kind": "OBJECT",
                "name": "test_var_pop_fields"
              }
            },
            {
              "args": [],
              "name": "var_samp",
              "type": {
                "kind": "OBJECT",
                "name": "test_var_samp_fields"
              }
            },
            {
              "args": [],
              "name": "variance",
              "type": {
                "kind": "OBJECT",
                "name": "test_variance_fields"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "avg",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_avg_order_by"
              }
            },
            {
              "name": "count",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            },
            {
              "name": "max",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_max_order_by"
              }
            },
            {
              "name": "min",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_min_order_by"
              }
            },
            {
              "name": "stddev",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_stddev_order_by"
              }
            },
            {
              "name": "stddev_pop",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_stddev_pop_order_by"
              }
            },
            {
              "name": "stddev_samp",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_stddev_samp_order_by"
              }
            },
            {
              "name": "sum",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_sum_order_by"
              }
            },
            {
              "name": "var_pop",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_var_pop_order_by"
              }
            },
            {
              "name": "var_samp",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_var_samp_order_by"
              }
            },
            {
              "name": "variance",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_variance_order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_aggregate_order_by",
          "description": "order by aggregate values of table \"test\""
        },
        {
          "inputFields": [
            {
              "name": "data",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "INPUT_OBJECT",
                      "name": "test_insert_input"
                    }
                  }
                }
              }
            },
            {
              "name": "on_conflict",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_on_conflict"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_arr_rel_insert_input",
          "description": "input type for inserting array relation for remote table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_avg_fields",
          "description": "aggregate avg on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Float"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_avg_order_by",
          "description": "order by avg() on columns of table \"test\""
        },
        {
          "inputFields": [
            {
              "name": "_and",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "INPUT_OBJECT",
                  "name": "test_bool_exp"
                }
              }
            },
            {
              "name": "_not",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_bool_exp"
              }
            },
            {
              "name": "_or",
              "type": {
                "kind": "LIST",
                "ofType": {
                  "kind": "INPUT_OBJECT",
                  "name": "test_bool_exp"
                }
              }
            },
            {
              "name": "id",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "integer_comparison_exp"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_bool_exp",
          "description": "Boolean expression to filter rows from the table \"test\". All fields are combined with a logical 'AND'."
        },
        {
          "kind": "ENUM",
          "name": "test_constraint",
          "enumValues": [
            {
              "name": "test_pkey",
              "description": "unique or primary key constraint"
            }
          ],
          "description": "unique or primary key constraints on table \"test\""
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_inc_input",
          "description": "input type for incrementing integer columne in table \"test\""
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_insert_input",
          "description": "input type for inserting data into table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_max_fields",
          "description": "aggregate max on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_max_order_by",
          "description": "order by max() on columns of table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_min_fields",
          "description": "aggregate min on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_min_order_by",
          "description": "order by min() on columns of table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_mutation_response",
          "description": "response of any mutation on the table \"test\"",
          "fields": [
            {
              "args": [],
              "name": "affected_rows",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "SCALAR",
                  "name": "Int"
                }
              },
              "description": "number of affected rows by the mutation"
            },
            {
              "args": [],
              "name": "returning",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "OBJECT",
                      "name": "test"
                    }
                  }
                }
              },
              "description": "data of the affected rows by the mutation"
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "data",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "INPUT_OBJECT",
                  "name": "test_insert_input"
                }
              }
            },
            {
              "name": "on_conflict",
              "type": {
                "kind": "INPUT_OBJECT",
                "name": "test_on_conflict"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_obj_rel_insert_input",
          "description": "input type for inserting object relation for remote table \"test\""
        },
        {
          "inputFields": [
            {
              "name": "constraint",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "ENUM",
                  "name": "test_constraint"
                }
              }
            },
            {
              "name": "update_columns",
              "type": {
                "kind": "NON_NULL",
                "ofType": {
                  "kind": "LIST",
                  "ofType": {
                    "kind": "NON_NULL",
                    "ofType": {
                      "kind": "ENUM",
                      "name": "test_update_column"
                    }
                  }
                }
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_on_conflict",
          "description": "on conflict condition type for table \"test\""
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_order_by",
          "description": "ordering options when selecting data from \"test\""
        },
        {
          "kind": "ENUM",
          "name": "test_select_column",
          "enumValues": [
            {
              "name": "id",
              "description": "column name"
            }
          ],
          "description": "select columns of table \"test\""
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_set_input",
          "description": "input type for updating data in table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_stddev_fields",
          "description": "aggregate stddev on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Float"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_stddev_order_by",
          "description": "order by stddev() on columns of table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_stddev_pop_fields",
          "description": "aggregate stddev_pop on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Float"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_stddev_pop_order_by",
          "description": "order by stddev_pop() on columns of table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_stddev_samp_fields",
          "description": "aggregate stddev_samp on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Float"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_stddev_samp_order_by",
          "description": "order by stddev_samp() on columns of table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_sum_fields",
          "description": "aggregate sum on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Int"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_sum_order_by",
          "description": "order by sum() on columns of table \"test\""
        },
        {
          "kind": "ENUM",
          "name": "test_update_column",
          "enumValues": [
            {
              "name": "id",
              "description": "column name"
            }
          ],
          "description": "update columns of table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_var_pop_fields",
          "description": "aggregate var_pop on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Float"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_var_pop_order_by",
          "description": "order by var_pop() on columns of table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_var_samp_fields",
          "description": "aggregate var_samp on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Float"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_var_samp_order_by",
          "description": "order by var_samp() on columns of table \"test\""
        },
        {
          "kind": "OBJECT",
          "interfaces": [],
          "name": "test_variance_fields",
          "description": "aggregate variance on columns",
          "fields": [
            {
              "args": [],
              "name": "id",
              "type": {
                "kind": "SCALAR",
                "name": "Float"
              }
            }
          ]
        },
        {
          "inputFields": [
            {
              "name": "id",
              "type": {
                "kind": "ENUM",
                "name": "order_by"
              }
            }
          ],
          "kind": "INPUT_OBJECT",
          "name": "test_variance_order_by",
          "description": "order by variance() on columns of table \"test\""
        }
      ],
      "mutationType": {
        "name": "mutation_root"
      }
    }
  }
}
|]
