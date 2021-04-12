module Hasura.GraphQL.Schema.Builder
  ( TyAgg(..)
  , FieldMap
  , taTypes
  , taFields
  , taScalars
  , taOrdBy
  , addFieldsToTyAgg
  , addTypeInfoToTyAgg
  , addScalarToTyAgg
  , QueryRootFieldMap
  , MutationRootFieldMap
  , RootFields(..)
  , addQueryField
  , addMutationField
  ) where

import           Control.Lens

import qualified Data.HashMap.Strict           as Map
import qualified Data.HashSet                  as Set
import qualified Language.GraphQL.Draft.Syntax as G

import           Hasura.GraphQL.Resolve.Types
import           Hasura.GraphQL.Validate.Types
import           Hasura.Prelude
import           Hasura.SQL.Types

-- | A /types aggregate/, which holds role-specific information about visible GraphQL types.
-- Importantly, it holds more than just the information needed by GraphQL: it also includes how the
-- GraphQL types relate to Postgres types, which is used to validate literals provided for
-- Postgres-specific scalars.
data TyAgg
  = TyAgg
  { _taTypes   :: !TypeMap
  , _taFields  :: !FieldMap
  , _taScalars :: !(Set.HashSet PGScalarType)
  , _taOrdBy   :: !OrdByCtx
  } deriving (Show, Eq)
$(makeLenses ''TyAgg)

addFieldsToTyAgg :: FieldMap -> TyAgg -> TyAgg
addFieldsToTyAgg fields =
  over taFields (Map.union fields)

addTypeInfoToTyAgg :: TypeInfo -> TyAgg -> TyAgg
addTypeInfoToTyAgg typeInfo tyAgg =
  tyAgg & taTypes.at (getNamedTy typeInfo) ?~ typeInfo

addScalarToTyAgg :: PGScalarType -> TyAgg -> TyAgg
addScalarToTyAgg pgScalarType =
  over taScalars (Set.insert pgScalarType)

instance Semigroup TyAgg where
  (TyAgg t1 f1 s1 o1) <> (TyAgg t2 f2 s2 o2) =
    TyAgg (Map.union t1 t2) (Map.union f1 f2)
          (Set.union s1 s2) (Map.union o1 o2)

instance Monoid TyAgg where
  mempty = TyAgg Map.empty Map.empty Set.empty Map.empty

type QueryRootFieldMap = Map.HashMap G.Name (QueryCtx, ObjFldInfo)
type MutationRootFieldMap = Map.HashMap G.Name (MutationCtx, ObjFldInfo)

-- | A role-specific mapping from root field names to allowed operations.
data RootFields
  = RootFields
  { _rootQueryFields    :: !QueryRootFieldMap
  , _rootMutationFields :: !MutationRootFieldMap
  } deriving (Show, Eq)
$(makeLenses ''RootFields)

addQueryField :: (QueryCtx, ObjFldInfo) -> RootFields -> RootFields
addQueryField v rootFields =
  rootFields & rootQueryFields.at (_fiName $ snd v) ?~ v

addMutationField :: (MutationCtx, ObjFldInfo) -> RootFields -> RootFields
addMutationField v rootFields =
  rootFields & rootMutationFields.at (_fiName $ snd v) ?~ v

instance Semigroup RootFields where
  RootFields a1 b1 <> RootFields a2 b2
    = RootFields (Map.union a1 a2) (Map.union b1 b2)

instance Monoid RootFields where
  mempty = RootFields Map.empty Map.empty
