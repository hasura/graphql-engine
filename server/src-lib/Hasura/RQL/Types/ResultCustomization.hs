module Hasura.RQL.Types.ResultCustomization
  ( AliasMapping,
    singletonAliasMapping,
    ResultCustomizer,
    applyResultCustomizer,
    applyAliasMapping,
    modifyFieldByName,
    customizeTypeNameString,
  )
where

import Data.Aeson.Ordered qualified as JO
import Data.HashMap.Strict as Map
import Data.Monoid (Endo (..))
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-- | Mapping that can be provided to a ResultCustomizer
-- to map top-level field aliases that were not available at field parse time.
-- E.g. for aliases created in the remote server query for remote joins.
newtype AliasMapping = AliasMapping {unAliasMapping :: Endo G.Name}
  deriving (Semigroup, Monoid)

-- | AliasMapping that maps a single field name to an alias
singletonAliasMapping :: G.Name -> G.Name -> AliasMapping
singletonAliasMapping fieldName alias = AliasMapping $
  Endo $ \fieldName' ->
    if fieldName == fieldName' then alias else fieldName'

-- | Function to modify JSON values returned from the remote server
-- e.g. to map values of __typename fields to customized type names.
-- The customizer uses Maybe to allow short-circuiting subtrees
-- where no customizations are needed.
newtype ResultCustomizer = ResultCustomizer {unResultCustomizer :: Maybe (AliasMapping -> Endo JO.Value)}
  deriving (Semigroup, Monoid)

-- | Apply a ResultCustomizer to a JSON value
applyResultCustomizer :: ResultCustomizer -> JO.Value -> JO.Value
applyResultCustomizer = maybe id (appEndo . ($ mempty)) . unResultCustomizer

-- | Apply an AliasMapping to a ResultCustomizer.
applyAliasMapping :: AliasMapping -> ResultCustomizer -> ResultCustomizer
applyAliasMapping aliasMapping (ResultCustomizer m) =
  ResultCustomizer $
    m <&> \g aliasMapping' -> g $ aliasMapping' <> aliasMapping

-- | Take a ResultCustomizer for a JSON subtree, and a fieldName,
-- and produce a ResultCustomizer for a parent object or array of objects
-- that applies the subtree customizer to the subtree at the given fieldName.
modifyFieldByName :: G.Name -> ResultCustomizer -> ResultCustomizer
modifyFieldByName fieldName (ResultCustomizer m) =
  ResultCustomizer $
    m <&> \g aliasMapping ->
      Endo $
        let Endo f = g mempty -- AliasMapping is only applied to the top level so use mempty for nested customizers
            modifyFieldByName' = \case
              JO.Object o -> JO.Object $ JO.adjust f (G.unName $ (appEndo $ unAliasMapping aliasMapping) fieldName) o
              JO.Array a -> JO.Array $ modifyFieldByName' <$> a
              v -> v
         in modifyFieldByName'

-- | Create a RemoteResultCustomizer that applies the typeNameMap
-- to a JSON string value, e.g. for use in customizing a __typename field value.
customizeTypeNameString :: HashMap G.Name G.Name -> ResultCustomizer
customizeTypeNameString typeNameMap =
  if Map.null typeNameMap
    then mempty
    else ResultCustomizer $
      Just $
        const $
          Endo $ \case
            JO.String t -> JO.String $ G.unName $ customizeTypeName $ G.unsafeMkName t
            v -> v
  where
    customizeTypeName :: G.Name -> G.Name
    customizeTypeName typeName = Map.lookupDefault typeName typeName typeNameMap
