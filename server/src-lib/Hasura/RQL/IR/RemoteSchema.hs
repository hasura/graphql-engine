{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}

-- | Normalized representation of GraphQL queries
--
-- The selection set of an incoming GraphQL query consists of fields, named
-- fragments, and inline fragments. However having named fragments in the IR
-- presents various challenges in the context of remote joins.
--
-- Hence, the parts of a GraphQL query that are serviced from remote GraphQL
-- servers have a simplified / normalized representation in the IR. Let's look
-- at the normalized variants of selection sets for various GraphQL types
--
-- 1. For scalars and enums, there is no selection set
--
-- 1. For object types, the selection set need not have any fragments, named or
-- inline. Any fragment defined on an object type can be recursively inlined
-- and merged to eventually end up with only 'fields'. So a selection set of
-- an object type can be represented as `Map Alias Field`.
--
-- 1. For abstract types such as interfaces and unions, the selection set can
-- be normalized to 'selection sets on concrete member types' (the
-- member types of interface and union types can only be object types as of
-- this writing). The haskell representation would be along these lines:
-- `Map ConcreteMemberTypeName (Map Alias Field)`
module Hasura.RQL.IR.RemoteSchema
  ( SelectionSet (..),
    convertSelectionSet,
    RemoteFieldG (..),
    GraphQLField (..),
    mkField,
    Field (..),
    ObjectSelectionSet,
    InterfaceSelectionSet,
    UnionSelectionSet,
    mkInterfaceSelectionSet,
    mkUnionSelectionSet,
    rfRemoteSchemaInfo,
    rfResultCustomizer,
    rfField,
    RemoteRootField (..),
    getRemoteFieldSelectionSet,
    RawRemoteField,
    RemoteField,
    realRemoteField,
    RemoteFieldArgument (..),
    RemoteSchemaSelect (..),
  )
where

import Control.Lens.TH (makeLenses)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Data.HashSet qualified as Set
import Hasura.GraphQL.Parser.Schema (InputValue)
import Hasura.Prelude
import Hasura.RQL.Types.Relationships.FromSource
import Hasura.RQL.Types.RemoteSchema
import Hasura.RQL.Types.RemoteSchema qualified as RQL
import Language.GraphQL.Draft.Syntax qualified as G

-- | A normalized representation of a GraphQL field
data GraphQLField r var = GraphQLField
  { _fAlias :: !G.Name,
    _fName :: !G.Name,
    _fArguments :: !(HashMap G.Name (G.Value var)),
    _fDirectives :: ![G.Directive var],
    _fSelectionSet :: !(SelectionSet r var)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

mkField ::
  Maybe G.Name ->
  G.Name ->
  HashMap G.Name (G.Value var) ->
  [G.Directive var] ->
  SelectionSet r var ->
  GraphQLField r var
mkField alias name =
  GraphQLField (fromMaybe name alias) name

type ObjectSelectionSet r var = OMap.InsOrdHashMap G.Name (Field r var)

data Field r var
  = FieldGraphQL !(GraphQLField r var)
  | FieldRemote !r
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | Represents the normalized selection set of an interface/union type
--
-- Note that this is slightly different than the `Map ConcreteMemberTypeName
-- (Map Alias Field)` structure described above. This is done to minimize the
-- size of the GraphQL query that eventually gets sent to the GraphQL server by
-- defining as many fields as possible on the abstract type
data AbstractTypeSelectionSet r var = AbstractTypeSelectionSet
  { -- | Fields that aren't explicitly defined for member types
    _atssCommonFields :: !(Set.HashSet G.Name),
    -- | SelectionSets of individual member types
    _atssMemberSelectionSets :: !(Map.HashMap G.Name (ObjectSelectionSet r var))
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

-- | Represents the normalized selection set of an interface/union type
--
-- Note that this is slightly different than the `Map ConcreteMemberTypeName
-- (Map Alias Field)` structure described above. This is done to minimize the
-- size of the GraphQL query that eventually gets sent to the GraphQL server by
-- defining as many fields as possible on the abstract type
-- data AbstractTypeSelectionSet r var = AbstractTypeSelectionSet
--   { -- | Fields that aren't explicitly defined for member types
--     _sssBaseSelectionSet :: !(ObjectSelectionSet r var),
--     -- | SelectionSets of individual member types
--     _sssMemberSelectionSets :: !(Map.HashMap G.Name (ObjectSelectionSet r var))
--   }
--   deriving (Show, Eq, Functor, Foldable, Traversable, Generic)
type InterfaceSelectionSet r var = AbstractTypeSelectionSet r var

type UnionSelectionSet r var = AbstractTypeSelectionSet r var

data SelectionSet r var
  = SelectionSetObject !(ObjectSelectionSet r var)
  | SelectionSetUnion !(UnionSelectionSet r var)
  | SelectionSetInterface !(InterfaceSelectionSet r var)
  | SelectionSetNone
  deriving (Show, Eq, Functor, Foldable, Traversable)

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
  AbstractTypeSelectionSet Void var ->
  (ObjectSelectionSet Void var, Map.HashMap G.Name (ObjectSelectionSet Void var))
reduceAbstractTypeSelectionSet (AbstractTypeSelectionSet baseMemberFields selectionSets) =
  (baseSelectionSet, Map.fromList memberSelectionSets)
  where
    sharedSelectionSetPrefix = sharedPrefix $ map (OMap.toList . snd) $ Map.toList selectionSets

    baseSelectionSet = OMap.fromList $ takeWhile (shouldAddToBase . snd) sharedSelectionSetPrefix

    shouldAddToBase = \case
      FieldGraphQL f -> Set.member (_fName f) baseMemberFields

    memberSelectionSets =
      -- remove member selection sets that are subsumed by base selection set
      filter (not . null . snd) $
        -- remove the common prefix from member selection sets
        map (second (OMap.fromList . drop (OMap.size baseSelectionSet) . OMap.toList)) $ Map.toList selectionSets

    sharedPrefix :: Eq a => [[a]] -> [a]
    sharedPrefix = \case
      [] -> []
      (x : xs) -> foldr prefix x xs
      where
        prefix l1 l2 = map fst $ takeWhile (uncurry (==)) $ zip l1 l2

mkInterfaceSelectionSet ::
  -- | Member fields of the interface
  Set.HashSet G.Name ->
  -- | Selection sets for all the member types
  [(G.Name, ObjectSelectionSet r var)] ->
  InterfaceSelectionSet r var
mkInterfaceSelectionSet interfaceFields selectionSets =
  AbstractTypeSelectionSet
    (Set.insert $$(G.litName "__typename") interfaceFields)
    (Map.fromList selectionSets)

mkUnionSelectionSet ::
  -- | Selection sets for all the member types
  [(G.Name, ObjectSelectionSet r var)] ->
  InterfaceSelectionSet r var
mkUnionSelectionSet selectionSets =
  AbstractTypeSelectionSet
    (Set.singleton $$(G.litName "__typename"))
    (Map.fromList selectionSets)

-- | Converts a normalized selection set into a selection set as defined in
-- GraphQL spec
convertSelectionSet :: (Eq var) => SelectionSet Void var -> G.SelectionSet G.NoFragments var
convertSelectionSet = \case
  SelectionSetObject s -> convertObjectSelectionSet s
  SelectionSetUnion s -> convertAbstractTypeSelectionSet s
  SelectionSetInterface s -> convertAbstractTypeSelectionSet s
  SelectionSetNone -> mempty
  where
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

    convertField :: (Eq var) => Field Void var -> G.Field G.NoFragments var
    convertField = \case
      FieldGraphQL f -> convertGraphQLField f

    convertObjectSelectionSet =
      map (G.SelectionField . convertField . snd) . OMap.toList

    convertAbstractTypeSelectionSet abstractSelectionSet =
      let (base, members) = reduceAbstractTypeSelectionSet abstractSelectionSet
          commonFields = convertObjectSelectionSet base
          concreteTypeSelectionSets =
            flip map (Map.toList members) \(concreteType, selectionSet) ->
              G.InlineFragment
                { G._ifTypeCondition = Just concreteType,
                  G._ifDirectives = mempty,
                  G._ifSelectionSet = convertObjectSelectionSet selectionSet
                }
       in -- The base selection set first and then the more specific member
          -- selection sets. Note that the rendering strategy here should be
          -- inline with the strategy used in `mkAbstractTypeSelectionSet`
          commonFields <> map G.SelectionInlineFragment concreteTypeSelectionSets

-- | A remote field annotated with information for execution phase
data RemoteFieldG f = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RQL.RemoteSchemaInfo,
    _rfResultCustomizer :: !RQL.RemoteResultCustomizer,
    _rfField :: !f
  }
  deriving (Functor, Foldable, Traversable)

-- | An RemoteRootField could either be a real field on the remote server
-- or represent a virtual namespace that only exists in the Hasura schema.
data RemoteRootField r var
  = -- | virtual namespace field
    RRFNamespaceField !(OMap.InsOrdHashMap G.Name (GraphQLField r var))
  | -- | a real field on the remote server
    RRFRealField !(GraphQLField r var)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | For a real remote field gives a SelectionSet for selecting the field itself.
--   For a virtual field gives the unwrapped SelectionSet for the field.
getRemoteFieldSelectionSet :: RemoteRootField r var -> ObjectSelectionSet r var
getRemoteFieldSelectionSet =
  fmap FieldGraphQL . \case
    RRFNamespaceField selSet -> selSet
    RRFRealField fld -> OMap.singleton (_fAlias fld) fld

type RawRemoteField r = RemoteFieldG (GraphQLField r RQL.RemoteSchemaVariable)

type RemoteField r = RemoteFieldG (RemoteRootField r RQL.RemoteSchemaVariable)

realRemoteField :: RawRemoteField r -> RemoteField r
realRemoteField RemoteFieldG {..} = RemoteFieldG {_rfField = RRFRealField _rfField, ..}

-- Remote schema relationships

data RemoteFieldArgument = RemoteFieldArgument
  { _rfaArgument :: !G.Name,
    _rfaValue :: !(InputValue RemoteSchemaVariable)
  }
  deriving (Eq, Show)

data RemoteSchemaSelect r = RemoteSchemaSelect
  { _rselArgs :: ![RemoteFieldArgument],
    _rselResultCustomizer :: !RemoteResultCustomizer,
    _rselSelection :: !(SelectionSet r RemoteSchemaVariable),
    _rselFieldCall :: !(NonEmpty FieldCall),
    _rselRemoteSchema :: !RemoteSchemaInfo
  }

$(makeLenses ''RemoteFieldG)
