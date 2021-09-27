{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hasura.RQL.IR.RemoteSchema
  ( SelectionSet (..),
    convertSelectionSet,
    RemoteFieldG (..),
    Field (..),
    ObjectSelectionSet,
    InterfaceSelectionSet,
    UnionSelectionSet,
    AbstractTypeSelectionSet,
    mkAbstractTypeSelectionSet,
    rfRemoteSchemaInfo,
    rfResultCustomizer,
    rfField,
    RemoteRootField (..),
    getRemoteFieldSelectionSet,
    RawRemoteField,
    RemoteField,
    realRemoteField,
  )
where

import Control.Lens.TH (makeLenses)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Hasura.GraphQL.Parser (Variable)
import Hasura.Prelude
import Hasura.RQL.Types.RemoteSchema qualified as RQL
import Language.GraphQL.Draft.Syntax qualified as G

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

-- | A normalized representation of a GraphQL field
data Field var = Field
  { _fName :: !G.Name,
    _fArguments :: !(HashMap G.Name (G.Value var)),
    _fDirectives :: ![G.Directive var],
    _fSelectionSet :: !(SelectionSet var)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

type ObjectSelectionSet var = OMap.InsOrdHashMap G.Name (Field var)

-- | Represents the normalized selection set of an interface/union type
--
-- Note that this is slightly different than `Map ConcreteMemberTypeName (Map
-- Alias Field)` to minimize the size of the rendered GraphQL query that
-- eventually gets sent to the GraphQL server
data AbstractTypeSelectionSet var = AbstractTypeSelectionSet
  { -- | Fields that aren't explicitly defined for member types
    _sssBaseSelectionSet :: !(ObjectSelectionSet var),
    -- | SelectionSets of individual member types
    _sssMemberSelectionSets :: !(Map.HashMap G.Name (ObjectSelectionSet var))
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

type InterfaceSelectionSet var = AbstractTypeSelectionSet var

type UnionSelectionSet var = AbstractTypeSelectionSet var

data SelectionSet var
  = SelectionSetObject !(ObjectSelectionSet var)
  | SelectionSetUnion !(UnionSelectionSet var)
  | SelectionSetInterface !(InterfaceSelectionSet var)
  | SelectionSetNone
  deriving (Show, Eq, Functor, Foldable, Traversable)

mkAbstractTypeSelectionSet ::
  Eq var =>
  (G.Name -> Bool) ->
  [(G.Name, ObjectSelectionSet var)] ->
  AbstractTypeSelectionSet var
mkAbstractTypeSelectionSet isBaseMember selectionSets =
  AbstractTypeSelectionSet
    { _sssBaseSelectionSet = baseSelectionSet,
      _sssMemberSelectionSets = Map.fromList memberSelectionSets
    }
  where
    sharedSelectionSetPrefix = sharedPrefix $ map (OMap.toList . snd) selectionSets
    baseSelectionSet = OMap.fromList $ takeWhile (isBaseMember . _fName . snd) sharedSelectionSetPrefix
    memberSelectionSets =
      -- remove member selection sets that are subsumed by base selection set
      filter (not . null . snd) $
        -- remove the common prefix from member selection sets
        map (second (OMap.fromList . drop (OMap.size baseSelectionSet) . OMap.toList)) selectionSets

    sharedPrefix :: Eq a => [[a]] -> [a]
    sharedPrefix = \case
      [] -> []
      (x : xs) -> foldr prefix x xs
      where
        prefix l1 l2 = map fst $ takeWhile (uncurry (==)) $ zip l1 l2

-- | Converts a normalized selection set into a selection set as defined in
-- GraphQL spec
convertSelectionSet :: SelectionSet var -> G.SelectionSet G.NoFragments var
convertSelectionSet = \case
  SelectionSetObject s -> convertObjectSelectionSet s
  SelectionSetUnion s -> convertAbstractTypeSelectionSet s
  SelectionSetInterface s -> convertAbstractTypeSelectionSet s
  SelectionSetNone -> mempty
  where

    convertField :: G.Name -> Field var -> G.Field G.NoFragments var
    convertField alias Field {..} =
      G.Field
        { G._fAlias = Just alias,
          G._fName = _fName,
          G._fArguments = _fArguments,
          G._fDirectives = mempty,
          G._fSelectionSet = convertSelectionSet _fSelectionSet
        }

    convertObjectSelectionSet =
      map (G.SelectionField . uncurry convertField) . OMap.toList

    convertAbstractTypeSelectionSet (AbstractTypeSelectionSet base members) =
      let commonFields = convertObjectSelectionSet base
          concreteTypeSelectionSets =
            flip map (Map.toList members) \(concreteType, selectionSet) ->
              G.InlineFragment
                { G._ifTypeCondition = Just concreteType,
                  G._ifDirectives = mempty,
                  G._ifSelectionSet = convertObjectSelectionSet selectionSet
                }
       in commonFields <> map G.SelectionInlineFragment concreteTypeSelectionSets

-- | A remote field annotated with information for execution phase
data RemoteFieldG f = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RQL.RemoteSchemaInfo,
    _rfResultCustomizer :: !RQL.RemoteResultCustomizer,
    _rfField :: !f
  }
  deriving (Functor, Foldable, Traversable)

-- | An RemoteRootField could either be a real field on the remote server
-- or represent a virtual namespace that only exists in the Hasura schema.
data RemoteRootField var
  = -- | virtual namespace field
    RRFNamespaceField !(ObjectSelectionSet var)
  | -- | a real field on the remote server
    RRFRealField !(Field var)
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | For a real remote field gives a SelectionSet for selecting the field itself.
--   For a virtual field gives the unwrapped SelectionSet for the field.
getRemoteFieldSelectionSet :: RemoteRootField Variable -> ObjectSelectionSet Variable
getRemoteFieldSelectionSet = \case
  RRFNamespaceField selSet -> selSet
  RRFRealField fld -> OMap.singleton (_fName fld) fld

type RawRemoteField = RemoteFieldG (Field RQL.RemoteSchemaVariable)

type RemoteField = RemoteFieldG (RemoteRootField RQL.RemoteSchemaVariable)

realRemoteField :: RawRemoteField -> RemoteField
realRemoteField RemoteFieldG {..} = RemoteFieldG {_rfField = RRFRealField _rfField, ..}

$(makeLenses ''RemoteFieldG)
