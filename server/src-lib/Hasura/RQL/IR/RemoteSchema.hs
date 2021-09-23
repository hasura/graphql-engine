{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Hasura.RQL.IR.RemoteSchema
  ( SelectionSet (..),
    convertSelectionSet,
    RemoteFieldG(..),
    rfRemoteSchemaInfo,
    rfResultCustomizer,
    rfField,
    RemoteRootField(..),
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
    ScopedSelectionSet (..),
    emptyScopedSelectionSet,
  )
where

import           Control.Lens.TH                (makeLenses)

import Hasura.Prelude
import Hasura.GraphQL.Parser (Variable)

import qualified Data.HashMap.Strict as Map
import qualified Data.HashMap.Strict.InsOrd.Extended as OMap
import qualified Hasura.RQL.Types.RemoteSchema as RQL
import qualified Language.GraphQL.Draft.Syntax as G

data RemoteFieldG f
  = RemoteFieldG
  { _rfRemoteSchemaInfo :: !RQL.RemoteSchemaInfo
  , _rfResultCustomizer :: !RQL.RemoteResultCustomizer
  , _rfField            :: !f
  } deriving (Functor, Foldable, Traversable)

$(makeLenses ''RemoteFieldG)

-- | An RemoteRootField could either be a real field on the remote server
-- or represent a virtual namespace that only exists in the Hasura schema.
data RemoteRootField var
  = RRFNamespaceField !(ObjectSelectionSet var) -- ^ virtual namespace field
  | RRFRealField !(Field var) -- ^ a real field on the remote server
  deriving (Show, Eq, Functor, Foldable, Traversable)

-- | For a real remote field gives a SelectionSet for selecting the field itself.
--   For a virtual field gives the unwrapped SelectionSet for the field.
getRemoteFieldSelectionSet :: RemoteRootField Variable -> G.SelectionSet G.NoFragments Variable
getRemoteFieldSelectionSet = convertSelectionSet . SelectionSetObject . \case
    RRFNamespaceField selSet -> selSet
    RRFRealField fld         -> OMap.singleton (_fName fld) fld

type RawRemoteField = RemoteFieldG (Field RQL.RemoteSchemaVariable)

type RemoteField = RemoteFieldG (RemoteRootField RQL.RemoteSchemaVariable)

realRemoteField :: RawRemoteField -> RemoteField
realRemoteField RemoteFieldG{..} = RemoteFieldG{_rfField = RRFRealField _rfField, ..}

-- | Ordered fields
type AliasedFields f = OMap.InsOrdHashMap G.Name f

type ObjectSelectionSet var = AliasedFields (Field var)

type ObjectSelectionSetMap var =
  Map.HashMap G.Name (ObjectSelectionSet var)

data Typename = Typename
  deriving (Show, Eq, Generic)

data ScopedSelectionSet f var
  = ScopedSelectionSet
  { -- | Fields that aren't explicitly defined for member types
    _sssBaseSelectionSet :: !(AliasedFields f)
    -- | SelectionSets of individual member types
  , _sssMemberSelectionSets :: !(ObjectSelectionSetMap var)
  } deriving (Show, Eq, Functor, Generic)

emptyScopedSelectionSet :: ScopedSelectionSet f var
emptyScopedSelectionSet =
  ScopedSelectionSet mempty mempty

type InterfaceSelectionSet var = ScopedSelectionSet (Field var) var

type UnionSelectionSet var = ScopedSelectionSet Typename var

data Field var = Field
  { _fName :: !G.Name
  , _fArguments :: !(HashMap G.Name (G.Value var))
  , _fDirectives :: ![G.Directive var]
  , _fSelectionSet :: !(SelectionSet var)
  } deriving (Show, Eq, Functor, Foldable, Traversable)

convertField :: Field Variable -> G.Field G.NoFragments Variable
convertField = undefined

convertSelectionSet :: SelectionSet var -> G.SelectionSet G.NoFragments var
convertSelectionSet = undefined

data SelectionSet var
  = SelectionSetObject !(ObjectSelectionSet var)
  | SelectionSetUnion !(UnionSelectionSet var)
  | SelectionSetInterface !(InterfaceSelectionSet var)
  | SelectionSetNone
  deriving (Show, Eq)

instance Functor SelectionSet where
  fmap f = \case
    SelectionSetObject s -> SelectionSetObject $ fmap (fmap f) s
    SelectionSetUnion s -> undefined
    SelectionSetInterface s -> undefined

instance Foldable SelectionSet where
instance Traversable SelectionSet where

