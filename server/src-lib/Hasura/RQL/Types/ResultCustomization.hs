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
import Data.HashMap.Strict as HashMap
import Data.Monoid (Endo (..))
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

-- | Mapping that can be provided to a ResultCustomizer
-- to map top-level field aliases that were not available at field parse time.
-- E.g. for aliases created in the remote server query for remote joins.
newtype AliasMapping = AliasMapping {unAliasMapping :: G.Name -> G.Name}
  deriving (Semigroup, Monoid) via (Endo G.Name)

-- | AliasMapping that maps a single field name to an alias
singletonAliasMapping :: G.Name -> G.Name -> AliasMapping
singletonAliasMapping fieldName alias = AliasMapping $ \fieldName' ->
  if fieldName == fieldName'
    then alias
    else fieldName'

-- | Function to modify JSON values returned from the remote server
-- e.g. to map values of __typename fields to customized type names.
-- The customizer uses Maybe to allow short-circuiting subtrees
-- where no customizations are needed.
newtype ResultCustomizer = ResultCustomizer {unResultCustomizer :: AliasMapping -> JO.Value -> JO.Value}
  deriving (Semigroup, Monoid) via (AliasMapping -> Endo JO.Value)

instance Show ResultCustomizer where
  show _ = "(ResultCustomizer <function>)"

-- | Apply a ResultCustomizer to a JSON value
applyResultCustomizer :: ResultCustomizer -> JO.Value -> JO.Value
applyResultCustomizer = ($ mempty) . unResultCustomizer

-- | Apply an AliasMapping to a ResultCustomizer.
applyAliasMapping :: AliasMapping -> ResultCustomizer -> ResultCustomizer
applyAliasMapping aliasMapping (ResultCustomizer m) =
  ResultCustomizer $ m . (<> aliasMapping)

-- | Take a ResultCustomizer for a JSON subtree, and a fieldName,
-- and produce a ResultCustomizer for a parent object or array of objects
-- that applies the subtree customizer to the subtree at the given fieldName.
modifyFieldByName :: G.Name -> ResultCustomizer -> ResultCustomizer
modifyFieldByName fieldName ResultCustomizer {..} =
  ResultCustomizer $ \AliasMapping {..} ->
    let applyCustomizer = unResultCustomizer mempty
        modifyFieldByName' = \case
          JO.Object o -> JO.Object $ JO.adjust applyCustomizer (G.unName $ unAliasMapping fieldName) o
          JO.Array a -> JO.Array $ modifyFieldByName' <$> a
          v -> v
     in modifyFieldByName'

-- | Create a RemoteResultCustomizer that applies the typeNameMap
-- to a JSON string value, e.g. for use in customizing a __typename field value.
customizeTypeNameString :: HashMap G.Name G.Name -> ResultCustomizer
customizeTypeNameString typeNameMap | HashMap.null typeNameMap = mempty
customizeTypeNameString typeNameMap =
  ResultCustomizer $ \_aliasMapping -> \case
    JO.String t -> JO.String
      $ fromMaybe t
      $ do
        -- This function is only meant to be applied on type names, and creating a
        -- GraphQL name out of the string should never fail. If it nonetheless
        -- fails, we assume there will not be customization information and we
        -- return it unmodified.
        typeName <- G.mkName t
        G.unName <$> HashMap.lookup typeName typeNameMap
    v -> v
