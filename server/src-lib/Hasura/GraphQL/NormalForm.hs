{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE RecordWildCards #-}
module Hasura.GraphQL.NormalForm
  ( Selection(..)
  , NormalizedSelection
  , NormalizedSelectionSet
  , NormalizedField
  , SelectionSet(..)
  , RootSelectionSet(..)
  , toGraphQLOperation
  , ArgsMap
  , Field(..)
  , Typename(..)
  , IsField(..)
  , toField
  , AliasedFields(..)
  , asObjectSelectionSet
  , ObjectSelectionSet(..)
  , ObjectSelectionSetMap
  , traverseObjectSelectionSet
  , InterfaceSelectionSet
  , getMemberSelectionSet
  , UnionSelectionSet
  , ScopedSelectionSet(..)
  , emptyScopedSelectionSet
  , getUnionSelectionSet
  , getInterfaceSelectionSet
  , getObjectSelectionSet

  , AnnInpVal(..)
  , AnnGValue(..)
  , AnnGObject
  , AnnGEnumValue(..)
  , hasNullVal
  , getAnnInpValKind

  , toGraphQLField
  , toGraphQLSelectionSet
  ) where

import           Hasura.Prelude

import qualified Data.Aeson                          as J
import qualified Data.Aeson.Casing                   as J
import qualified Data.Aeson.TH                       as J
import qualified Data.HashMap.Strict                 as Map
import qualified Data.HashMap.Strict.InsOrd.Extended as OMap
import qualified Language.GraphQL.Draft.Syntax       as G

import           Hasura.SQL.Types
import           Hasura.SQL.Value
import qualified Hasura.RQL.Types.Column       as RQL
import qualified Hasura.RQL.Types.Error        as RQL

data Selection f s
  = SelectionField !G.Alias !f
  | SelectionInlineFragmentSpread !s
  | SelectionFragmentSpread !G.Name !s
  deriving (Show, Eq)

-- | What a processed G.SelectionSet should look like
type family NormalizedSelectionSet a = s | s -> a

-- | What a processed G.Field should look like
type family NormalizedField a

type NormalizedSelection a
  = Selection (NormalizedField a) (NormalizedSelectionSet a)

-- | Ordered fields
newtype AliasedFields f
  = AliasedFields { unAliasedFields :: OMap.InsOrdHashMap G.Alias f }
  deriving (Show, Eq, Functor, Foldable, Traversable, Semigroup)

newtype ObjectSelectionSet
  = ObjectSelectionSet { unObjectSelectionSet :: AliasedFields Field }
  deriving (Show, Eq, Semigroup)

traverseObjectSelectionSet
  :: (Monad m) => ObjectSelectionSet -> (Field -> m a) -> m [(Text, a)]
traverseObjectSelectionSet selectionSet f =
  forM (OMap.toList $ unAliasedFields $ unObjectSelectionSet selectionSet) $
    \(alias, field) -> (G.unName $ G.unAlias alias,) <$> f field

type ObjectSelectionSetMap
  = Map.HashMap G.NamedType ObjectSelectionSet

data Typename = Typename
  deriving (Show, Eq, Generic)

data ScopedSelectionSet f
  = ScopedSelectionSet
  { _sssBaseSelectionSet    :: !(AliasedFields f)
  -- ^ Fields that aren't explicitly defined for member types
  , _sssMemberSelectionSets :: !ObjectSelectionSetMap
  -- ^ SelectionSets of individual member types
  } deriving (Show, Eq, Generic)



emptyScopedSelectionSet :: ScopedSelectionSet f
emptyScopedSelectionSet =
  ScopedSelectionSet (AliasedFields mempty) mempty

type InterfaceSelectionSet = ScopedSelectionSet Field

type UnionSelectionSet = ScopedSelectionSet Typename

data RootSelectionSet
  = RQuery !ObjectSelectionSet
  | RMutation !ObjectSelectionSet
  | RSubscription !G.Alias !Field
  deriving (Show, Eq)

toGraphQLOperation :: RootSelectionSet -> G.ExecutableDefinition
toGraphQLOperation = \case
  RQuery selectionSet ->
    mkExecutableDefinition G.OperationTypeQuery $
    toGraphQLSelectionSet $ SelectionSetObject selectionSet
  RMutation selectionSet ->
    mkExecutableDefinition G.OperationTypeQuery $
    toGraphQLSelectionSet $ SelectionSetObject selectionSet
  RSubscription alias field ->
    mkExecutableDefinition G.OperationTypeSubscription $
    toGraphQLSelectionSet $ SelectionSetObject $ ObjectSelectionSet $
      AliasedFields $ OMap.singleton alias field
  where
    mkExecutableDefinition operationType selectionSet =
      G.ExecutableDefinitionOperation $ G.OperationDefinitionTyped $
      G.TypedOperationDefinition
        { G._todName = Nothing -- TODO, store the name too?
        , G._todDirectives = []
        , G._todType = operationType
        , G._todVariableDefinitions = []
        , G._todSelectionSet = selectionSet
        }


data SelectionSet
  = SelectionSetObject !ObjectSelectionSet
  | SelectionSetUnion !UnionSelectionSet
  | SelectionSetInterface !InterfaceSelectionSet
  | SelectionSetNone
  -- ^ in cases of enums and scalars
  deriving (Show, Eq)

getObjectSelectionSet :: SelectionSet -> Maybe ObjectSelectionSet
getObjectSelectionSet = \case
  SelectionSetObject s -> pure s
  _ -> Nothing

asObjectSelectionSet
  :: (MonadError RQL.QErr m) => SelectionSet -> m ObjectSelectionSet
asObjectSelectionSet selectionSet =
  onNothing (getObjectSelectionSet selectionSet) $
    RQL.throw500 "expecting ObjectSelectionSet"

getUnionSelectionSet :: SelectionSet -> Maybe UnionSelectionSet
getUnionSelectionSet = \case
  SelectionSetUnion s -> pure s
  _ -> Nothing

getInterfaceSelectionSet :: SelectionSet -> Maybe InterfaceSelectionSet
getInterfaceSelectionSet = \case
  SelectionSetInterface s -> pure s
  _ -> Nothing

type ArgsMap = Map.HashMap G.Name AnnInpVal

data Field
  = Field
  { _fName      :: !G.Name
  , _fType      :: !G.NamedType
  , _fArguments :: !ArgsMap
  , _fSelSet    :: !SelectionSet
  } deriving (Eq, Show)

toGraphQLField :: G.Alias -> Field -> G.Field
toGraphQLField alias Field{..} =
  G.Field
    { G._fName = _fName
    , G._fArguments = [] -- TODO
    , G._fDirectives = []
    , G._fAlias = Just alias
    , G._fSelectionSet =  toGraphQLSelectionSet _fSelSet
    }

toGraphQLSelectionSet :: SelectionSet -> G.SelectionSet
toGraphQLSelectionSet = \case
  SelectionSetObject selectionSet -> fromSelectionSet selectionSet
  SelectionSetInterface selectionSet -> fromScopedSelectionSet selectionSet
  SelectionSetUnion selectionSet -> fromScopedSelectionSet selectionSet
  SelectionSetNone -> mempty
  where
    fromAliasedFields :: (IsField f) => AliasedFields f -> G.SelectionSet
    fromAliasedFields =
      map (G.SelectionField . uncurry toGraphQLField) .
      OMap.toList . fmap toField . unAliasedFields
    fromSelectionSet =
      fromAliasedFields . unObjectSelectionSet
    toInlineSelection typeName =
      G.SelectionInlineFragment . G.InlineFragment (Just typeName) mempty .
      fromSelectionSet
    fromScopedSelectionSet (ScopedSelectionSet base specific) =
      map (uncurry toInlineSelection) (Map.toList specific) <> fromAliasedFields base

-- $(J.deriveToJSON (J.aesonDrop 2 J.camelCase){J.omitNothingFields=True}
--   ''Field
--  )

-- $(J.deriveToJSON (J.aesonDrop 2 J.camelCase){J.omitNothingFields=True}
--   ''InterfaceSelectionSet
--  )

-- $(J.deriveToJSON (J.aesonDrop 2 J.camelCase){J.omitNothingFields=True}
--   ''SelectionSet
--  )

class IsField f where
  getFieldName :: f -> G.Name
  getFieldType :: f -> G.NamedType
  getFieldArguments :: f -> ArgsMap
  getFieldSelectionSet :: f -> SelectionSet

toField :: (IsField f) => f -> Field
toField f =
  Field (getFieldName f) (getFieldType f)
  (getFieldArguments f) (getFieldSelectionSet f)

instance IsField Field where
  getFieldName = _fName
  getFieldType = _fType
  getFieldArguments = _fArguments
  getFieldSelectionSet = _fSelSet

instance IsField Typename where
  getFieldName _ = "__typename"
  getFieldType _ = G.NamedType "String"
  getFieldArguments _ = mempty
  getFieldSelectionSet _ = SelectionSetNone

getMemberSelectionSet
  :: IsField f
  => G.NamedType -> ScopedSelectionSet f -> ObjectSelectionSet
getMemberSelectionSet namedType (ScopedSelectionSet {..})  =
  fromMaybe (ObjectSelectionSet (fmap toField _sssBaseSelectionSet)) $
  Map.lookup namedType $ _sssMemberSelectionSets

data AnnInpVal
  = AnnInpVal
  { _aivType     :: !G.GType
  , _aivVariable :: !(Maybe G.Variable)
  , _aivValue    :: !AnnGValue
  } deriving (Show, Eq)

type AnnGObject = OMap.InsOrdHashMap G.Name AnnInpVal

-- | See 'EnumValuesInfo' for information about what these cases mean.
data AnnGEnumValue
  = AGESynthetic !(Maybe G.EnumValue)
  | AGEReference !RQL.EnumReference !(Maybe RQL.EnumValue)
  deriving (Show, Eq)

data AnnGValue
  = AGScalar !PGScalarType !(Maybe PGScalarValue)
  | AGEnum !G.NamedType !AnnGEnumValue
  | AGObject !G.NamedType !(Maybe AnnGObject)
  | AGArray !G.ListType !(Maybe [AnnInpVal])
  deriving (Show, Eq)

$(J.deriveToJSON (J.aesonDrop 4 J.camelCase){J.omitNothingFields=True}
  ''AnnInpVal
 )

instance J.ToJSON AnnGValue where
  -- toJSON (AGScalar ty valM) =
  toJSON = const J.Null
    -- J.
    -- J.toJSON [J.toJSON ty, J.toJSON valM]

hasNullVal :: AnnGValue -> Bool
hasNullVal = \case
  AGScalar _ Nothing                -> True
  AGEnum _ (AGESynthetic Nothing)   -> True
  AGEnum _ (AGEReference _ Nothing) -> True
  AGObject _ Nothing                -> True
  AGArray _ Nothing                 -> True
  _                                 -> False

getAnnInpValKind :: AnnGValue -> Text
getAnnInpValKind = \case
  AGScalar _ _ -> "scalar"
  AGEnum _ _   -> "enum"
  AGObject _ _ -> "object"
  AGArray _ _  -> "array"
