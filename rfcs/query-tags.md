---
authors: Naveenaidu <naveen@hasura.io>
state: draft
---

# Query Tags:

## Motivation

We need some ability for some users who are using native monitoring tools (pganalyze, RDS performance insights) to see some "application context" in the query logs. We should be able to give `key=value` pairs as defined by the spec [here](https://google.github.io/sqlcommenter/spec/#format). This is a PRO only feature.

## Approach

The main idea is to convert the `<ATTRIBUTE_KEY_VALUE_PAIRS>` as SQL comment and append this to the SQL statement that is generated when a GraphQL operation is run using Hasura. 

Since there is various nitty-gritty with each vendor (who support automatic visualization) with the query tags format so, in the interest of (future) customization, we will need to introduce a configuration element to the metadata.

### Creating MonadQueryTags Monad Class

Query Tags is a Pro-only feature. Thus we need a new Monad Class to segregate the implementation between OSS and Pro

```haskell
-- The (Key, Value) pair where key corresponds to the name of the query tag.
type Attribute = (Text, Text)

-- Convert the list of attributes to SQL Comment made up of Query Tags 
 sqlCommenter :: [Attribute] -> Text

data QueryTagsSourceConfig
  = QueryTagsSourceConfig
  { _qtscFormat   :: !QueryTagsFormat
  , _qtscDisabled :: !Bool
  } deriving (Show, Eq, Generic)

-- The QueryTagsSourceConfig comes from the metadata. This is used to configure the query tags for each source
class (Monad m) => MonadQueryTags m where
  -- | Creates Query Tags. These are appened to the Generated SQL.
  -- Helps users to use native database monitoring tools to get some 'application-context'.
  createQueryTags
    :: (Maybe QueryTagsSourceConfig) -> [Attribute] -> Tagged m Text

-- For Pro, appened the query tags to the prepared SQL statement
instance HGE.MonadQueryTags (AppM impl) where
  createQueryTags qtSourceConfig attributes = 
    case qtSourceConfig of
      -- 'Nothing' case match exists, because RQL queries and normal GQL queries both call the same function
      -- which needs the queryTags. And we do not append any 'QueryTags' to 'RQL Queries'
      Nothing -> mempty
      -- When Query Tags is disabled, do not create query tags
      Just (HGE.QueryTagsSourceConfig _ True) -> mempty
      Just (HGE.QueryTagsSourceConfig HGE.SQLCommenter False) -> return $ sqlCommenter attributes
      Just (HGE.QueryTagsSourceConfig HGE.Standard False) -> return $ standardSQLCommenter attributes

-- For OSS, return the SQL without any modification
instance (Monad m) => MonadQueryTags (PGMetadataStorageAppT m) where
  createQueryTags _qtSourceConfig _attributes = mempty

```

The QueryTags are generated where the `mkDB..Plan` functions are called since all the information necessary to make the QueryTags are available there. The Query Tags are converted as SQL commment at the same place and passed down the stack where the SQL is generated for the GQL operation. The Query Tags are then appened to the SQL.

### Note:

The above approach is only the first iteration of query tags. 

We initially planned to have `appendQueryTags` inside the `BackendExecute` typeclass but that did not work out well as the `appendQueryTags` would then depend on the `PreparedQuery b` type family which is not used by mutations and for some backends, it only gets created after pretty-printing. 
