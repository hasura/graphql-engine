module Hasura.GraphQL.Namespace
  ( RootFieldAlias (..),
    mkUnNamespacedRootFieldAlias,
    mkNamespacedRootFieldAlias,
    RootFieldMap,
    NamespacedField (..),
    namespacedField,
    NamespacedFieldMap,
    flattenNamespaces,
    unflattenNamespaces,
    customizeNamespace,
  )
where

import Data.Aeson qualified as J
import Data.HashMap.Strict.InsOrd qualified as InsOrdHashMap
import Data.Text.Extended
import Hasura.GraphQL.Schema.Parser as P
import Hasura.GraphQL.Schema.Typename
import Hasura.Prelude
import Language.GraphQL.Draft.Syntax qualified as G

data RootFieldAlias = RootFieldAlias
  { _rfaNamespace :: !(Maybe G.Name),
    _rfaAlias :: !G.Name
  }
  deriving (Show, Eq, Generic)

instance Hashable RootFieldAlias

instance ToTxt RootFieldAlias where
  toTxt RootFieldAlias {..} = case _rfaNamespace of
    Nothing -> G.unName _rfaAlias
    Just ns -> G.unName ns <> "." <> G.unName _rfaAlias

-- | This ToJSON instance is used in responses to the explain API
-- (via the ToJSON instance for ExplainPlan).
-- It will use dot separator for namespaces fields, i.e. "namespace.fieldname"
-- TODO: We need to decide if this dotted notation is what we want to use for explain responses.
instance J.ToJSON RootFieldAlias where
  toJSON = J.toJSON . toTxt

mkUnNamespacedRootFieldAlias :: G.Name -> RootFieldAlias
mkUnNamespacedRootFieldAlias = RootFieldAlias Nothing

mkNamespacedRootFieldAlias :: G.Name -> G.Name -> RootFieldAlias
mkNamespacedRootFieldAlias = RootFieldAlias . Just

type RootFieldMap = InsOrdHashMap RootFieldAlias

data NamespacedField a
  = -- | Normal field
    NotNamespaced a
  | -- | Namespace field with other fields nested within
    Namespaced (InsOrdHashMap G.Name a)
  deriving (Eq, Show, Functor)

namespacedField :: (a -> b) -> (InsOrdHashMap G.Name a -> b) -> NamespacedField a -> b
namespacedField f g = \case
  NotNamespaced a -> f a
  Namespaced m -> g m

type NamespacedFieldMap a = InsOrdHashMap G.Name (NamespacedField a)

flattenNamespaces :: forall a. NamespacedFieldMap a -> RootFieldMap a
flattenNamespaces = InsOrdHashMap.foldMapWithKey flattenNamespace
  where
    flattenNamespace :: G.Name -> NamespacedField a -> RootFieldMap a
    flattenNamespace fieldName =
      namespacedField
        (InsOrdHashMap.singleton $ mkUnNamespacedRootFieldAlias fieldName)
        (InsOrdHashMap.mapKeys $ mkNamespacedRootFieldAlias fieldName)

unflattenNamespaces :: RootFieldMap a -> NamespacedFieldMap a
unflattenNamespaces = InsOrdHashMap.foldlWithKey' insert mempty
  where
    insert m RootFieldAlias {..} v = case _rfaNamespace of
      Nothing -> InsOrdHashMap.insert _rfaAlias (NotNamespaced v) m
      Just ns -> InsOrdHashMap.insertWith merge ns (Namespaced $ (InsOrdHashMap.singleton _rfaAlias v)) m
    merge (Namespaced m) (Namespaced m') = Namespaced (InsOrdHashMap.union m' m) -- Note: order of arguments to InsOrdHashMap.union to preserve ordering
    merge v _ = v

-- | Wrap the field parser results in @NamespacedField@
customizeNamespace ::
  forall n a.
  (MonadParse n) =>
  Maybe G.Name ->
  (G.Name -> P.ParsedSelection a -> a) ->
  MkTypename ->
  [FieldParser n a] ->
  [FieldParser n (NamespacedField a)]
customizeNamespace (Just _) _ _ [] = [] -- The nampespace doesn't contain any Field parsers, so returning empty list
customizeNamespace (Just namespace) fromParsedSelection mkNamespaceTypename fieldParsers =
  -- Source or remote schema has a namespace field so wrap the parsers
  -- in a new namespace field parser.
  [P.subselection_ namespace Nothing parser]
  where
    parser :: Parser 'Output n (NamespacedField a)
    parser =
      Namespaced
        . InsOrdHashMap.mapWithKey fromParsedSelection
        <$> P.selectionSet (runMkTypename mkNamespaceTypename namespace) Nothing fieldParsers
customizeNamespace Nothing _ _ fieldParsers =
  -- No namespace so just wrap the field parser results in @NotNamespaced@.
  fmap NotNamespaced <$> fieldParsers
