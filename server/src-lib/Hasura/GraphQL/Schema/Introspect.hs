module Hasura.GraphQL.Schema.Introspect where

import           Hasura.Prelude
-- import qualified Hasura.RQL.Types

import qualified Language.GraphQL.Draft.Syntax as G
import qualified Data.Aeson                    as J
-- import qualified Data.HashMap.Strict           as Map
import qualified Data.HashMap.Strict.InsOrd    as OMap
import           Data.Void
import qualified Data.Vector                   as V

import qualified Hasura.GraphQL.Parser         as P

import           Hasura.GraphQL.Parser         (FieldParser, Kind (..), Parser)
import           Hasura.GraphQL.Parser.Class

data Schema = Schema
  { sDescription :: Maybe G.Description
  -- TODO add support for input objects as well. NB 'Both does not suffice as
  -- this is the intersection of 'Input and 'Output, rather than its union.
  , sTypes :: [P.Type 'Output]
  , sQueryType :: P.Type 'Output
  , sMutationType :: Maybe (P.Type 'Output)
  , sSubscriptionType :: Maybe (P.Type 'Output)
  , sDirectives :: [G.Directive Void]
  }

-- | Takes an existing @Parser@ that takes GraphQL queries to Postgres queries,
-- and generates the introspection parser.
schema
  :: forall n
   . MonadParse n
  => Schema
  -> FieldParser n J.Value
schema realSchema =
  let schemaSetParser = schemaSet realSchema
  in P.subselection_ $$(G.litName "__schema") Nothing schemaSetParser

{-
type __Type {
  kind: __TypeKind!
  name: String
  description: String

  # should be non-null for OBJECT and INTERFACE only, must be null for the others
  fields(includeDeprecated: Boolean = false): [__Field!]

  # should be non-null for OBJECT and INTERFACE only, must be null for the others
  interfaces: [__Type!]

  # should be non-null for INTERFACE and UNION only, always null for the others
  possibleTypes: [__Type!]

  # should be non-null for ENUM only, must be null for the others
  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]

  # should be non-null for INPUT_OBJECT only, must be null for the others
  inputFields: [__InputValue!]

  # should be non-null for NON_NULL and LIST only, must be null for the others
  ofType: __Type
}
-}
typeField
  :: forall n k
   . MonadParse n
  => P.Type k
  -> Parser 'Output n J.Value
typeField to = do
  let
    to' = case to of
      P.NonNullable x -> x
      P.Nullable x -> x
    name :: FieldParser n J.Value
    name = P.selection_ $$(G.litName "name") Nothing P.string $>
      case to' of
        P.TList _ -> J.Null
        P.TNamed def -> mkTypeName (P.dName def)
    description :: FieldParser n J.Value
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      case to' of
        P.TList _ -> J.Null
        P.TNamed def -> case (P.dDescription def) of
          Nothing -> J.Null
          Just desc -> J.String (G.unDescription desc)
    fields :: FieldParser n J.Value
    fields = do
      -- TODO includeDeprecated
      printer <- P.subselection_ $$(G.litName "fields") Nothing fieldField
      return $ case to' of
        P.TList _ -> J.Null
        P.TNamed def -> case P.dInfo def of
          P.TIObject fields' -> do
            J.Array $ V.fromList $ fmap printer fields'
          _ -> J.Null -- TODO ghc says this is redundant?
  fmap (J.Object . OMap.toHashMap . (OMap.mapKeys G.unName) . fmap (P.handleTypename mkTypeName)) $
    P.selectionSet
      $$(G.litName "type")
      Nothing
      [ name
      , description
      , fields
      ]

{-
type __Field {
  name: String!
  description: String
  args: [__InputValue!]!
  type: __Type!
  isDeprecated: Boolean!
  deprecationReason: String
}
-}
fieldField
  :: forall n
   . MonadParse n
  => Parser 'Output n (P.Definition P.FieldInfo -> J.Value)
fieldField =
  let
    name :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    name = P.selection_ $$(G.litName "name") Nothing P.string $> mkTypeName . P.dName
    description :: FieldParser n (P.Definition P.FieldInfo -> J.Value)
    description = P.selection_ $$(G.litName "description") Nothing P.string $> \defInfo ->
      case P.dDescription defInfo of
      Nothing -> J.Null
      Just desc -> J.String (G.unDescription desc)
  in
    fmap (\omap -> \defInfo -> J.Object $ OMap.toHashMap $ (OMap.mapKeys G.unName) $ fmap ($ defInfo) $ fmap (P.handleTypename (\name' _def -> mkTypeName name')) $ omap) $
    P.selectionSet $$(G.litName "__field") Nothing
    [ name
    , description
    ]

-- uhh this is wrong
mkTypeName :: G.Name -> J.Value
mkTypeName = J.String . G.unName
{-
type __Directive {
  name: String!
  description: String
  locations: [__DirectiveLocation!]!
  args: [__InputValue!]!
  isRepeatable: Boolean!
}
-}

-- TODO actually output data
directiveSet
  :: forall n
   . MonadParse n
  => Parser 'Output n J.Value
directiveSet =
  let
    name :: FieldParser n J.Value
    name = J.Null <$ P.selection_ $$(G.litName "name") Nothing P.string
    description :: FieldParser n J.Value
    description = J.Null <$ P.selection_ $$(G.litName "description") Nothing P.string
    locations :: FieldParser n J.Value
    locations = J.Null <$ P.selection_ $$(G.litName "locations") Nothing P.string
    args :: FieldParser n J.Value
    args = J.Null <$ P.subselection_ $$(G.litName "args") Nothing (P.selectionSet undefined Nothing [])
    isRepeatable :: FieldParser n J.Value
    isRepeatable = J.Null <$ P.selection_ $$(G.litName "isRepeatable") Nothing P.string
  in
    -- fmap (J.Object . OMap.toHashMap . (OMap.mapKeys G.unName) . fmap (P.handleTypename mkTypeName)) $
    J.Null <$ P.selectionSet $$(G.litName "__directive") Nothing
    [ name
    , description
    , locations
    , args
    , isRepeatable
    ]

{-
type __Schema {
  description: String
  types: [__Type!]!
  queryType: __Type!
  mutationType: __Type
  subscriptionType: __Type
  directives: [__Directive!]!
}
-}

schemaSet
  :: forall n
   . MonadParse n
  => Schema
  -> Parser 'Output n J.Value
schemaSet realSchema =
  let
    description :: FieldParser n J.Value
    description = P.selection_ $$(G.litName "description") Nothing P.string $>
      case sDescription realSchema of
        Nothing -> J.Null
        Just s -> J.String $ G.unDescription s

    types :: FieldParser n J.Value
    types = J.Array mempty <$ P.subselection_ $$(G.litName "types") Nothing (typeField undefined)
    queryType :: FieldParser n J.Value
    queryType = P.subselection_ $$(G.litName "queryType") Nothing (typeField (sQueryType realSchema))
    directives :: FieldParser n J.Value
    directives = J.Array mempty <$ P.subselection_ $$(G.litName "directives") Nothing directiveSet
    -- TODO implement
    mutationType :: FieldParser n J.Value
    mutationType = J.Null <$ P.subselection_ $$(G.litName "mutationType") Nothing (typeField undefined)
    subscriptionType :: FieldParser n J.Value
    subscriptionType = J.Null <$ P.subselection_ $$(G.litName "subscriptionType") Nothing (typeField undefined)
  in
    fmap (J.Object . OMap.toHashMap . (OMap.mapKeys G.unName) . fmap (P.handleTypename mkTypeName)) $
    P.selectionSet
      $$(G.litName "__schema")
      Nothing
      [ description
      , types
      , queryType
      , mutationType
      , subscriptionType
      , directives
      ]
