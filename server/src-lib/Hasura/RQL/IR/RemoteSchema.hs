{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hasura.RQL.IR.RemoteSchema
  ( SelectionSet (..),
    convertSelectionSet,
    RemoteFieldG (..),
    rfRemoteSchemaInfo,
    rfResultCustomizer,
    rfField,
    RemoteRootField (..),
    getRemoteFieldSelectionSet,
    RawRemoteField,
    RemoteField,
    realRemoteField,
    Field (..),
    Typename (..),
    AliasedFields,
    ObjectSelectionSet,
    InterfaceSelectionSet,
    UnionSelectionSet,
    ScopedSelectionSet,
    mkScopedSelectionSet,
    emptyScopedSelectionSet,
  )
where

import Control.Lens.TH (makeLenses)
import Data.HashMap.Strict qualified as Map
import Data.HashMap.Strict.InsOrd.Extended qualified as OMap
import Hasura.GraphQL.Parser (Variable)
import Hasura.Prelude
import Hasura.RQL.Types.RemoteSchema qualified as RQL
import Language.GraphQL.Draft.Syntax qualified as G

data RemoteFieldG f = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RQL.RemoteSchemaInfo,
    _rfResultCustomizer :: !RQL.RemoteResultCustomizer,
    _rfField :: !f
  }
  deriving (Functor, Foldable, Traversable)

$(makeLenses ''RemoteFieldG)

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
getRemoteFieldSelectionSet :: RemoteRootField Variable -> G.SelectionSet G.NoFragments Variable
getRemoteFieldSelectionSet =
  convertSelectionSet . SelectionSetObject . \case
    RRFNamespaceField selSet -> selSet
    RRFRealField fld -> OMap.singleton (_fName fld) fld

type RawRemoteField = RemoteFieldG (Field RQL.RemoteSchemaVariable)

type RemoteField = RemoteFieldG (RemoteRootField RQL.RemoteSchemaVariable)

realRemoteField :: RawRemoteField -> RemoteField
realRemoteField RemoteFieldG {..} = RemoteFieldG {_rfField = RRFRealField _rfField, ..}

-- | Ordered fields
type AliasedFields var = OMap.InsOrdHashMap G.Name (Field var)

type ObjectSelectionSet var = AliasedFields var

type ObjectSelectionSetMap var =
  Map.HashMap G.Name (ObjectSelectionSet var)

data Typename = Typename
  deriving (Show, Eq, Generic)

data ScopedSelectionSet var = ScopedSelectionSet
  { -- | Fields that aren't explicitly defined for member types
    _sssBaseSelectionSet :: !(AliasedFields var),
    -- | SelectionSets of individual member types
    _sssMemberSelectionSets :: !(ObjectSelectionSetMap var)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable, Generic)

mkScopedSelectionSet :: Eq var => (G.Name -> Bool) -> [(G.Name, ObjectSelectionSet var)] -> ScopedSelectionSet var
mkScopedSelectionSet isBaseMember selectionSets =
  ScopedSelectionSet
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

emptyScopedSelectionSet :: ScopedSelectionSet var
emptyScopedSelectionSet =
  ScopedSelectionSet mempty mempty

type InterfaceSelectionSet var = ScopedSelectionSet var

type UnionSelectionSet var = ScopedSelectionSet var

data Field var = Field
  { _fName :: !G.Name,
    _fArguments :: !(HashMap G.Name (G.Value var)),
    _fDirectives :: ![G.Directive var],
    _fSelectionSet :: !(SelectionSet var)
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)

convertField :: G.Name -> Field var -> G.Field G.NoFragments var
convertField alias Field {..} =
  G.Field
    { G._fAlias = Just alias,
      G._fName = _fName,
      G._fArguments = _fArguments,
      G._fDirectives = mempty,
      G._fSelectionSet = convertSelectionSet _fSelectionSet
    }

convertSelectionSet :: SelectionSet var -> G.SelectionSet G.NoFragments var
convertSelectionSet = \case
  SelectionSetObject s -> convertObjectSelectionSet s
  SelectionSetUnion s -> convertScopedSelectionSet s
  SelectionSetInterface s -> convertScopedSelectionSet s
  SelectionSetNone -> mempty
  where
    convertAliasedFields aliasedFields =
      map (uncurry convertField) $ OMap.toList aliasedFields

    convertObjectSelectionSet =
      map G.SelectionField . convertAliasedFields

    convertScopedSelectionSet (ScopedSelectionSet base members) =
      let commonFields = map G.SelectionField $ convertAliasedFields base
          concreteTypeSelectionSets =
            flip map (Map.toList members) \(concreteType, selectionSet) ->
              G.InlineFragment
                { G._ifTypeCondition = Just concreteType,
                  G._ifDirectives = mempty,
                  G._ifSelectionSet = convertObjectSelectionSet selectionSet
                }
       in commonFields <> map G.SelectionInlineFragment concreteTypeSelectionSets

data SelectionSet var
  = SelectionSetObject !(ObjectSelectionSet var)
  | SelectionSetUnion !(UnionSelectionSet var)
  | SelectionSetInterface !(InterfaceSelectionSet var)
  | SelectionSetNone
  deriving (Show, Eq, Functor, Foldable, Traversable)
