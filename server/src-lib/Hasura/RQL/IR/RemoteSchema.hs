{-# LANGUAGE TemplateHaskell #-}

-- | Representation for queries going to remote schemas. Due to the existence of
-- remote relationships from remote schemas, we can't simply reuse the GraphQL
-- document AST we define in graphql-parser-hs, and instead redefine a custom
-- structure to represent such queries.
module Hasura.RQL.IR.RemoteSchema
  ( -- AST
    SelectionSet (..),
    DeduplicatedSelectionSet (..),
    dssCommonFields,
    dssMemberSelectionSets,
    ObjectSelectionSet,
    mkInterfaceSelectionSet,
    mkUnionSelectionSet,
    Field (..),
    _FieldGraphQL,
    _FieldRemote,
    GraphQLField (..),
    fAlias,
    fName,
    fArguments,
    fDirectives,
    fSelectionSet,
    mkGraphQLField,
    -- entry points
    RemoteSchemaRootField (..),
    SchemaRemoteRelationshipSelect (..),
    RemoteFieldArgument (..),
    RemoteSchemaSelect (..),
    -- AST conversion
    convertSelectionSet,
    convertGraphQLField,
  )
where

import Control.Lens.TH (makeLenses, makePrisms)
import Data.HashMap.Strict qualified as HashMap
import Data.HashMap.Strict.InsOrd.Extended qualified as InsOrdHashMap
import Data.HashSet qualified as Set
import Data.List.Extended (longestCommonPrefix)
import Hasura.GraphQL.Parser.Name as GName
import Hasura.GraphQL.Parser.Variable (InputValue)
import Hasura.Prelude
import Hasura.RQL.Types.Common (FieldName)
import Hasura.RQL.Types.ResultCustomization
import Hasura.RQL.Types.ResultCustomization qualified as RQL
import Hasura.RemoteSchema.SchemaCache.Types
import Language.GraphQL.Draft.Syntax qualified as G

-------------------------------------------------------------------------------
-- Custom AST

-- | Custom representation of a selection set.
--
-- Similarly to other parts of the IR, the @r@ argument is used for remote
-- relationships.
data SelectionSet r var
  = SelectionSetObject (ObjectSelectionSet r var)
  | SelectionSetUnion (DeduplicatedSelectionSet r var)
  | SelectionSetInterface (DeduplicatedSelectionSet r var)
  | SelectionSetNone
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Representation of the normalized selection set of an interface/union type.
--
-- This representation is used to attempt to minimize the size of the GraphQL
-- query that eventually gets sent to the GraphQL server by defining as many
-- fields as possible on the abstract type.
data DeduplicatedSelectionSet r var = DeduplicatedSelectionSet
  { -- | Fields that aren't explicitly defined for member types
    _dssCommonFields :: Set.HashSet G.Name,
    -- | SelectionSets of individual member types
    _dssMemberSelectionSets :: HashMap.HashMap G.Name (ObjectSelectionSet r var)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

type ObjectSelectionSet r var = InsOrdHashMap.InsOrdHashMap G.Name (Field r var)

-- | Constructs an 'InterfaceSelectionSet' from a set of interface fields and an
-- association list of the fields. This function ensures that @__typename@ is
-- present in the set of interface fields.
mkInterfaceSelectionSet ::
  -- | Member fields of the interface
  Set.HashSet G.Name ->
  -- | Selection sets for all the member types
  [(G.Name, ObjectSelectionSet r var)] ->
  DeduplicatedSelectionSet r var
mkInterfaceSelectionSet interfaceFields selectionSets =
  DeduplicatedSelectionSet
    (Set.insert GName.___typename interfaceFields)
    (HashMap.fromList selectionSets)

-- | Constructs an 'UnionSelectionSet' from a list of the fields, using a
-- singleton set of @__typename@ for the set of common fields.
mkUnionSelectionSet ::
  -- | Selection sets for all the member types
  [(G.Name, ObjectSelectionSet r var)] ->
  DeduplicatedSelectionSet r var
mkUnionSelectionSet selectionSets =
  DeduplicatedSelectionSet
    (Set.singleton GName.___typename)
    (HashMap.fromList selectionSets)

-- | Representation of one individual field.
--
-- This particular type is the reason why we need a different representation
-- from the one in 'graphql-parser-hs': we differentiate between selection
-- fields that target the actual remote schema, and fields that, instead, are
-- remote from it and need to be treated differently.
data Field r var
  = FieldGraphQL (GraphQLField r var)
  | FieldRemote (SchemaRemoteRelationshipSelect r)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Normalized representation of a GraphQL field.
--
-- This type is almost identical to 'G.Field', except for the fact that the
-- selection set is our annotated 'SelectionSet', instead of the original
-- 'G.SelectionSet'. We use this type to represent the fields of a selection
-- that do target the remote schema.
data GraphQLField r var = GraphQLField
  { _fAlias :: G.Name,
    _fName :: G.Name,
    _fArguments :: HashMap G.Name (G.Value var),
    _fDirectives :: [G.Directive var],
    _fSelectionSet :: SelectionSet r var
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

mkGraphQLField ::
  Maybe G.Name ->
  G.Name ->
  HashMap G.Name (G.Value var) ->
  [G.Directive var] ->
  SelectionSet r var ->
  GraphQLField r var
mkGraphQLField alias name =
  GraphQLField (fromMaybe name alias) name

-------------------------------------------------------------------------------
-- Remote schema entry points

-- | Root entry point for a remote schema.
data RemoteSchemaRootField r var = RemoteSchemaRootField
  { _rfRemoteSchemaInfo :: RemoteSchemaInfo,
    _rfResultCustomizer :: RQL.ResultCustomizer,
    _rfField :: GraphQLField r var
  }
  deriving (Functor, Foldable, Traversable)

-- | A remote relationship's selection and fields required for its join condition.
data SchemaRemoteRelationshipSelect r = SchemaRemoteRelationshipSelect
  { -- | The fields on the table that are required for the join condition
    -- of the remote relationship
    _srrsLHSJoinFields :: HashMap FieldName G.Name,
    -- | The field that captures the relationship
    -- r ~ (RemoteRelationshipField UnpreparedValue) when the AST is emitted by the parser.
    -- r ~ Void when an execution tree is constructed so that a backend is
    -- absolved of dealing with remote relationships.
    _srrsRelationship :: r
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

data RemoteFieldArgument = RemoteFieldArgument
  { _rfaArgument :: G.Name,
    _rfaValue :: InputValue RemoteSchemaVariable
  }
  deriving (Eq, Show)

data RemoteSchemaSelect r = RemoteSchemaSelect
  { _rselArgs :: [RemoteFieldArgument],
    _rselResultCustomizer :: ResultCustomizer,
    _rselSelection :: SelectionSet r RemoteSchemaVariable,
    _rselFieldCall :: NonEmpty FieldCall,
    _rselRemoteSchema :: RemoteSchemaInfo
  }
  deriving (Show)

-------------------------------------------------------------------------------
-- Conversion back to a GraphQL document

-- | Converts a normalized selection set back into a selection set as defined in
-- GraphQL spec, in order to send it to a remote server.
--
-- This function expects a 'SelectionSet' for which @r@ is 'Void', which
-- guarantees that there is no longer any remote join field in the selection
-- set.
convertSelectionSet ::
  forall var.
  (Eq var) =>
  SelectionSet Void var ->
  G.SelectionSet G.NoFragments var
convertSelectionSet = \case
  SelectionSetObject s -> convertObjectSelectionSet s
  SelectionSetUnion s -> convertAbstractTypeSelectionSet s
  SelectionSetInterface s -> convertAbstractTypeSelectionSet s
  SelectionSetNone -> mempty
  where
    convertField :: Field Void var -> G.Field G.NoFragments var
    convertField = \case
      FieldGraphQL f -> convertGraphQLField f

    convertObjectSelectionSet =
      map (G.SelectionField . convertField . snd) . InsOrdHashMap.toList

    convertAbstractTypeSelectionSet abstractSelectionSet =
      let (base, members) = reduceAbstractTypeSelectionSet abstractSelectionSet
          commonFields = convertObjectSelectionSet base
          concreteTypeSelectionSets =
            HashMap.toList members <&> \(concreteType, selectionSet) ->
              G.InlineFragment
                { G._ifTypeCondition = Just concreteType,
                  G._ifDirectives = mempty,
                  G._ifSelectionSet = convertObjectSelectionSet selectionSet
                }
       in -- The base selection set first and then the more specific member
          -- selection sets. Note that the rendering strategy here should be
          -- inline with the strategy used in `mkAbstractTypeSelectionSet`
          commonFields <> map G.SelectionInlineFragment concreteTypeSelectionSets

convertGraphQLField :: (Eq var) => GraphQLField Void var -> G.Field G.NoFragments var
convertGraphQLField GraphQLField {..} =
  G.Field
    { -- add the alias only if it is different from the field name. This
      -- keeps the outbound request more readable
      G._fAlias = if _fAlias /= _fName then Just _fAlias else Nothing,
      G._fName = _fName,
      G._fArguments = _fArguments,
      G._fDirectives = mempty,
      G._fSelectionSet = convertSelectionSet _fSelectionSet
    }

-- | Builds the selection set for an abstract type.
--
-- Let's consider this query on starwars API:
-- The type `Node` an interface is implemented by `Film`, `Species`, `Planet`,
-- `Person`, `Starship`, `Vehicle`
--
-- query f {
--   node(id: "ZmlsbXM6MQ==") {
--     __typename
--     id
--     ... on Film {
--       title
--     }
--     ... on Species {
--       name
--     }
--   }
-- }
--
-- When we parse this, it gets normalized into this query:
--
-- query f {
--   node(id: "ZmlsbXM6MQ==") {
--     ... on Film {
--       __typename: __typename
--       id
--       title
--     }
--     ... on Species {
--       __typename: __typename
--       id
--       name
--     }
--     ... on Planet {
--       __typename: __typename
--       id
--     }
--     ... on Person {
--       __typename: __typename
--       id
--     }
--     ... on Starship {
--       __typename: __typename
--       id
--     }
--     ... on Vehicle {
--       __typename: __typename
--       id
--     }
--   }
-- }
--
-- `__typename` and `id` get pushed to each of the member types. From the above
-- normalized selection set, we want to costruct a query as close to the
-- original as possible. We do this as follows:
--
-- 1. find the longest common set of fields that each selection set starts with
--    (in the above case, they are `__typename` and `id`)
-- 2. from the above list of fields, find the first field that cannot be
--    defined on the abstract type. The fields that can be defined on the
--    abstract type are all the fields that occur before the first non abstract
--    type field (in the above case, both` __typename` and `id` can be defined
--    on the `Node` type)
-- 3. Strip the base selection set fields from all the member selection sets and
--    filter out the member type selection sets that are subsumed by the base
--    selection set
--
-- The above query now translates to this:
--
-- query f {
--   node(id: "ZmlsbXM6MQ==") {
--     __typename: __typename
--     id
--     ... on Film {
--       title
--     }
--     ... on Species {
--       name
--     }
--   }
-- }
--
-- Note that it is not always possible to get the same shape as the original
-- query and there is more than one approach to this. For example, we could
-- have picked the selection set (that can be defined on the abstract type)
-- that is common across all the member selection sets and used that as the
-- base selection.
reduceAbstractTypeSelectionSet ::
  (Eq var) =>
  DeduplicatedSelectionSet Void var ->
  (ObjectSelectionSet Void var, HashMap.HashMap G.Name (ObjectSelectionSet Void var))
reduceAbstractTypeSelectionSet (DeduplicatedSelectionSet baseMemberFields selectionSets) =
  (baseSelectionSet, HashMap.fromList memberSelectionSets)
  where
    sharedSelectionSetPrefix = longestCommonPrefix $ map (InsOrdHashMap.toList . snd) $ HashMap.toList selectionSets

    baseSelectionSet = InsOrdHashMap.fromList $ takeWhile (shouldAddToBase . snd) sharedSelectionSetPrefix

    shouldAddToBase = \case
      FieldGraphQL f -> Set.member (_fName f) baseMemberFields

    memberSelectionSets =
      -- remove member selection sets that are subsumed by base selection set
      filter (not . null . snd)
        $
        -- remove the common prefix from member selection sets
        map (second (InsOrdHashMap.fromList . drop (InsOrdHashMap.size baseSelectionSet) . InsOrdHashMap.toList))
        $ HashMap.toList selectionSets

-------------------------------------------------------------------------------
-- TH lens generation

$(makePrisms ''Field)
$(makeLenses ''GraphQLField)
$(makeLenses ''DeduplicatedSelectionSet)
