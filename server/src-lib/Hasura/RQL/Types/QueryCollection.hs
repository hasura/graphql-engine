module Hasura.RQL.Types.QueryCollection
  ( CollectionName
  , CollectionDef(..)
  , CreateCollection(..)
  , AddQueryToCollection(..)
  , DropQueryFromCollection(..)
  , DropCollection(..)
  , CollectionReq(..)
  , GQLQuery(..)
  , GQLQueryWithText(..)
  , QueryName(..)
  , ListedQuery(..)
  , getGQLQuery
  , queryWithoutTypeNames
  ) where

import           Hasura.Incremental            (Cacheable)
import           Hasura.Prelude
import           Hasura.RQL.Instances          ()
import           Hasura.RQL.Types.Common       (NonEmptyText)
import           Hasura.SQL.Types

import           Data.Aeson
import           Data.Aeson.Casing
import           Data.Aeson.TH
import           Language.Haskell.TH.Syntax    (Lift)

import qualified Data.Text                     as T
import qualified Database.PG.Query             as Q
import qualified Language.GraphQL.Draft.Syntax as G

newtype CollectionName
  = CollectionName {unCollectionName :: NonEmptyText}
  deriving ( Show, Eq, Ord, Hashable, ToJSON, ToJSONKey, Lift
           , FromJSON, Q.FromCol, Q.ToPrepArg, DQuote
           , Generic, Arbitrary
           )

newtype QueryName
  = QueryName {unQueryName :: NonEmptyText}
  deriving (Show, Eq, Ord, NFData, Hashable, Lift, ToJSON, ToJSONKey, FromJSON, DQuote, Generic, Arbitrary, Cacheable)

newtype GQLQuery
  = GQLQuery { unGQLQuery :: G.ExecutableDocument G.Name }
  deriving (Show, Eq, NFData, Hashable, Lift, ToJSON, FromJSON, Cacheable)

newtype GQLQueryWithText
  = GQLQueryWithText (T.Text, GQLQuery)
  deriving (Show, Eq, NFData, Lift, Generic, Cacheable)

instance FromJSON GQLQueryWithText where
  parseJSON v@(String t) = GQLQueryWithText . (t, ) <$> parseJSON v
  parseJSON _            = fail "expecting String for GraphQL query"

instance ToJSON GQLQueryWithText where
  toJSON (GQLQueryWithText (t, _)) = String t

getGQLQuery :: GQLQueryWithText -> GQLQuery
getGQLQuery (GQLQueryWithText v) = snd v

queryWithoutTypeNames :: GQLQuery -> GQLQuery
queryWithoutTypeNames =
  GQLQuery . G.ExecutableDocument . stripTypenames
  . G.getExecutableDefinitions . unGQLQuery

-- WIP NOTE
-- this was lifted from Validate. Should this be here?
stripTypenames :: forall var. [G.ExecutableDefinition var] -> [G.ExecutableDefinition var]
stripTypenames = map filterExecDef
  where
    filterExecDef :: G.ExecutableDefinition var -> G.ExecutableDefinition var
    filterExecDef = \case
      G.ExecutableDefinitionOperation opDef  ->
        G.ExecutableDefinitionOperation $ filterOpDef opDef
      G.ExecutableDefinitionFragment fragDef ->
        let newSelset = filterSelSet $ G._fdSelectionSet fragDef
        in G.ExecutableDefinitionFragment fragDef{G._fdSelectionSet = newSelset}

    filterOpDef  = \case
      G.OperationDefinitionTyped typeOpDef ->
        let newSelset = filterSelSet $ G._todSelectionSet typeOpDef
        in G.OperationDefinitionTyped typeOpDef{G._todSelectionSet = newSelset}
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
          in Just $ G.SelectionField  f{G._fSelectionSet = newSelset}
      _                  -> Just s


data ListedQuery
  = ListedQuery
  { _lqName  :: !QueryName
  , _lqQuery :: !GQLQueryWithText
  } deriving (Show, Eq, Lift, Generic)
instance NFData ListedQuery
instance Cacheable ListedQuery
$(deriveJSON (aesonDrop 3 snakeCase) ''ListedQuery)

type QueryList = [ListedQuery]

newtype CollectionDef
  = CollectionDef
  { _cdQueries :: QueryList }
  deriving (Show, Eq, Lift, Generic, NFData, Cacheable)
$(deriveJSON (aesonDrop 3 snakeCase) ''CollectionDef)

data CreateCollection
  = CreateCollection
  { _ccName       :: !CollectionName
  , _ccDefinition :: !CollectionDef
  , _ccComment    :: !(Maybe T.Text)
  } deriving (Show, Eq, Lift, Generic)
$(deriveJSON (aesonDrop 3 snakeCase) ''CreateCollection)

data DropCollection
  = DropCollection
  { _dcCollection :: !CollectionName
  , _dcCascade    :: !Bool
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 3 snakeCase) ''DropCollection)

data AddQueryToCollection
  = AddQueryToCollection
  { _aqtcCollectionName :: !CollectionName
  , _aqtcQueryName      :: !QueryName
  , _aqtcQuery          :: !GQLQueryWithText
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 5 snakeCase) ''AddQueryToCollection)

data DropQueryFromCollection
  = DropQueryFromCollection
  { _dqfcCollectionName :: !CollectionName
  , _dqfcQueryName      :: !QueryName
  } deriving (Show, Eq, Lift)
$(deriveJSON (aesonDrop 5 snakeCase) ''DropQueryFromCollection)

newtype CollectionReq
  = CollectionReq
  {_crCollection :: CollectionName}
  deriving (Show, Eq, Lift, Generic)
$(deriveJSON (aesonDrop 3 snakeCase) ''CollectionReq)
