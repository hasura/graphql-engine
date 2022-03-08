module Hasura.RQL.Types.Allowlist
  ( -- | The schema cache representation of the allowlist
    InlinedAllowlist (..),
    inlineAllowlist,
    AllowlistMode (..),
    allowlistAllowsQuery,
    -- | The normalised metadata representation of the allowlist
    AllowlistEntry (..),
    UpdateScopeOfCollectionInAllowlist (..),
    MetadataAllowlist,
    DropCollectionFromAllowlist (..),
    AllowlistScope (..),
    metadataAllowlistInsert,
    metadataAllowlistUpdateScope,
    metadataAllowlistAllCollections,
    NormalizedQuery (..),
  )
where

import Data.Aeson
import Data.Aeson.TH (deriveJSON, deriveToJSON)
import Data.HashMap.Strict.Extended qualified as M
import Data.HashMap.Strict.InsOrd.Extended qualified as OM
import Data.HashSet qualified as S
import Data.Text.Extended ((<<>))
import Hasura.Prelude
import Hasura.RQL.Types.QueryCollection
import Hasura.Session (RoleName)
import Language.GraphQL.Draft.Syntax qualified as G

newtype DropCollectionFromAllowlist = DropCollectionFromAllowlist
  { _dcfaCollection :: CollectionName
  }
  deriving (Show, Eq)

$(deriveJSON hasuraJSON ''DropCollectionFromAllowlist)

data AllowlistScope
  = AllowlistScopeGlobal
  | AllowlistScopeRoles !(NonEmpty RoleName)
  deriving (Show, Eq, Generic)

instance FromJSON AllowlistScope where
  parseJSON = withObject "AllowlistScope" $ \o -> do
    global <- o .: "global"
    if global
      then do
        roles :: Maybe (NonEmpty RoleName) <- o .:? "roles"
        case roles of
          Nothing -> pure AllowlistScopeGlobal
          Just {} -> fail "roles are not allowed when global is true"
      else do
        roles <- o .: "roles"
        if (length roles /= length (S.fromList (toList roles)))
          then fail "duplicate roles are not allowed"
          else pure $ AllowlistScopeRoles roles

instance ToJSON AllowlistScope where
  toJSON scope = case scope of
    AllowlistScopeGlobal -> object ["global" .= True]
    AllowlistScopeRoles roles -> object ["global" .= False, "roles" .= roles]

data AllowlistEntry = AllowlistEntry
  { aeCollection :: !CollectionName,
    aeScope :: !AllowlistScope
  }
  deriving (Show, Eq, Generic)

$(deriveToJSON hasuraJSON ''AllowlistEntry)

instance FromJSON AllowlistEntry where
  parseJSON = withObject "AllowlistEntry" \o -> do
    collectionName <- o .: "collection"
    scope <- o .:? "scope" .!= AllowlistScopeGlobal
    pure $ AllowlistEntry collectionName scope

-- | Wrap 'AllowlistEntry' with a FromJSON instance that requires 'scope' to be set.
newtype UpdateScopeOfCollectionInAllowlist = UpdateScopeOfCollectionInAllowlist AllowlistEntry

instance FromJSON UpdateScopeOfCollectionInAllowlist where
  parseJSON = withObject "UpdateScopeOfCollectionInAllowlist" \o -> do
    collectionName <- o .: "collection"
    scope <- o .: "scope"
    pure $ UpdateScopeOfCollectionInAllowlist $ AllowlistEntry collectionName scope

type MetadataAllowlist = InsOrdHashMap CollectionName AllowlistEntry

metadataAllowlistInsert ::
  AllowlistEntry -> MetadataAllowlist -> Either Text MetadataAllowlist
metadataAllowlistInsert entry@(AllowlistEntry coll _) al =
  OM.alterF insertIfAbsent coll al
  where
    insertIfAbsent = \case
      Nothing -> Right (Just entry)
      Just _ ->
        Left $
          "collection " <> coll <<> " already exists in the allowlist, scope ignored;"
            <> " to change scope, use update_scope_of_collection_in_allowlist"

metadataAllowlistUpdateScope ::
  AllowlistEntry -> MetadataAllowlist -> Either Text MetadataAllowlist
metadataAllowlistUpdateScope entry@(AllowlistEntry coll _) al =
  OM.alterF setIfPresent coll al
  where
    setIfPresent = \case
      Just _ -> Right (Just entry)
      Nothing -> Left $ "collection " <> coll <<> " doesn't exist in the allowlist"

-- | Produce a list of all collections in the allowlist.
-- This is used in 'runDropCollection' to function to ensure that we don't delete
-- any collections which are referred to in the allowlist.
metadataAllowlistAllCollections :: MetadataAllowlist -> [CollectionName]
metadataAllowlistAllCollections = toList . OM.map aeCollection

-- | A query stripped of typenames. A query is allowed if it occurs
-- in an allowed query collection after normalization.
--
-- Compare docs/graphql/core/deployment/allow-list.rst.
newtype NormalizedQuery = NormalizedQuery {unNormalizedQuery :: G.ExecutableDocument G.Name}
  deriving (Show, Eq, Hashable, ToJSON)

-- | Normalize query for comparison by stripping type names.
normalizeQuery :: G.ExecutableDocument G.Name -> NormalizedQuery
normalizeQuery =
  NormalizedQuery
    . G.ExecutableDocument
    . map filterExecDef
    . G.getExecutableDefinitions
  where
    filterExecDef :: G.ExecutableDefinition var -> G.ExecutableDefinition var
    filterExecDef = \case
      G.ExecutableDefinitionOperation opDef ->
        G.ExecutableDefinitionOperation $ filterOpDef opDef
      G.ExecutableDefinitionFragment fragDef ->
        let newSelset = filterSelSet $ G._fdSelectionSet fragDef
         in G.ExecutableDefinitionFragment fragDef {G._fdSelectionSet = newSelset}

    filterOpDef = \case
      G.OperationDefinitionTyped typeOpDef ->
        let newSelset = filterSelSet $ G._todSelectionSet typeOpDef
         in G.OperationDefinitionTyped typeOpDef {G._todSelectionSet = newSelset}
      G.OperationDefinitionUnTyped selset ->
        G.OperationDefinitionUnTyped $ filterSelSet selset

    filterSelSet :: [G.Selection frag var'] -> [G.Selection frag var']
    filterSelSet = mapMaybe filterSel

    filterSel :: G.Selection frag var' -> Maybe (G.Selection frag var')
    filterSel s = case s of
      G.SelectionField f ->
        if G._fName f == $$(G.litName "__typename")
          then Nothing
          else
            let newSelset = filterSelSet $ G._fSelectionSet f
             in Just $ G.SelectionField f {G._fSelectionSet = newSelset}
      _ -> Just s

-- | InlinedAllowlist is the data type with which the allowlist is represented
--   in the schema cache, it contains a global and a per role allowlist and when
--   allowlist is enabled in the graphql-engine, the incoming query for a non-admin
--   role should either be in the global allowlist or in the given role's role
--   based allowlist.
--
--   Essentially, it's a memoization of 'allowlistAllowsQuery' implemented
--   in terms of 'MetadataAllowlist'.
data InlinedAllowlist = InlinedAllowlist
  { iaGlobal :: HashSet NormalizedQuery,
    iaPerRole :: HashMap RoleName (HashSet NormalizedQuery)
  }
  deriving (Show, Eq)

$(deriveToJSON hasuraJSON ''InlinedAllowlist)

inlineAllowlist :: QueryCollections -> MetadataAllowlist -> InlinedAllowlist
inlineAllowlist collections allowlist = InlinedAllowlist global perRole
  where
    globalCollections :: [CollectionName]
    globalCollections =
      [coll | AllowlistEntry coll AllowlistScopeGlobal <- OM.elems allowlist]
    perRoleCollections :: HashMap RoleName [CollectionName]
    perRoleCollections =
      inverseMap $
        [ (coll, toList roles)
          | AllowlistEntry coll (AllowlistScopeRoles roles) <- OM.elems allowlist
        ]

    inverseMap :: (Eq b, Hashable b) => [(a, [b])] -> HashMap b [a]
    inverseMap = M.fromListWith (<>) . concatMap (\(c, rs) -> [(r, [c]) | r <- rs])

    global = inlineQueries globalCollections
    perRole = inlineQueries <$> perRoleCollections

    -- given a hashset of collections, look up what queries are in each
    -- collection, and inline them all into a hashset of queries
    inlineQueries :: [CollectionName] -> HashSet NormalizedQuery
    inlineQueries =
      concatMap lookupQueries
        >>> map normalizeQuery
        >>> S.fromList

    lookupQueries :: CollectionName -> [G.ExecutableDocument G.Name]
    lookupQueries coll =
      maybe [] collectionQueries $ OM.lookup coll collections

-- | The mode in which the allowlist functions. In global mode,
-- collections with non-global scope are ignored.
data AllowlistMode = AllowlistModeGlobalOnly | AllowlistModeFull

allowlistAllowsQuery ::
  InlinedAllowlist -> AllowlistMode -> RoleName -> G.ExecutableDocument G.Name -> Bool
allowlistAllowsQuery (InlinedAllowlist global perRole) mode role query =
  case mode of
    AllowlistModeGlobalOnly -> inAllowlist global
    AllowlistModeFull -> inAllowlist global || inAllowlist roleAllowlist
  where
    inAllowlist = S.member (normalizeQuery query)
    roleAllowlist = M.findWithDefault mempty role perRole
