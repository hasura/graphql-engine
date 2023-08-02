-- |
-- This module calculates parameterized query hash, which is a way to
-- hash an incoming query (after resolving variables) with all leaf nodes
-- (i.e. scalar values) discarded. In other words, two queries having the same
-- parameterized query hash are essentially the same query but may differ in
-- leaf values.
--
-- For example:
--
-- 1. query {
--      authors (where: {id: {_eq: 2}}) {
--        id
--        name
--      }
--    }
--
-- 2. query {
--      authors (where: {id: {_eq: 203943}}) {
--        id
--        name
--      }
--    }
--
-- 3. query {
--      authors (where: {id: {_eq: $id}}) {
--        id
--        name
--      }
--    }
--
--   For any value of `id`
--
-- 4. query {
--      authors (where: $whereBoolExp) {
--        id
--        name
--      }
--    }
--
--    only when `whereBoolExp` is of the form of
--
--    {
--       "id": {
--         "_eq": <id>
--       }
--    }
--
-- All the above queries should result in the same parameterized query hash.
--
-- The following steps are done to calculate the parameterized query hash:
--
-- 1. Normalize the GraphQL query by substituting the variables (if any) in appropriate places.
-- 2. Substitute any scalar GraphQL values (Int, Float, Enum, String and Boolean) to null
-- 3. For input objects and list, traverse through them and do step no 2.
-- 4. Calculate the hash of the query obtained from step 3.
--
-- Note: Parameterized query hash is a PRO only feature
module Hasura.GraphQL.ParameterizedQueryHash
  ( calculateParameterizedQueryHash,
    mkUnsafeParameterizedQueryHash,
    unParamQueryHash,
    ParameterizedQueryHash,
    ParameterizedQueryHashList (..),
    parameterizedQueryHashListToObject,
  )
where

import Data.Aeson qualified as J
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as B
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable (hashWithSalt))
import Hasura.GraphQL.Parser (InputValue (..), Variable (..))
import Hasura.Prelude
import Hasura.Server.Utils (cryptoHash)
import Language.GraphQL.Draft.Printer qualified as G
import Language.GraphQL.Draft.Syntax qualified as G
import Text.Builder qualified as Text

-- | a set of parameterized query hashes attached to a request
-- this type exists because a simple list of 'ParameterisedQueryHash'es won't
-- let us log a single-request batch and a single non-batched request
-- differently. the log format uses json lists for requests executed in batched
-- mode, for fields like @query@, but not for requests in single mode (e.g.
-- @query: "..."@ vs @query: ["..."]@) and so to conform to that, we capture the
-- whole _set_ of parameterised query hashes when it's created, tagging it with
-- information about how it was created (i.e. from a batched request, a single
-- request, etc.)
data ParameterizedQueryHashList
  = -- | an empty query hash set, either for an operation that does not produce
    -- query hashes, or due to failure in operation execution
    PQHSetEmpty
  | -- | a query hash set consisting of a single element, corresponding to e.g.
    -- a single (non-batched) graphql request
    PQHSetSingleton !ParameterizedQueryHash
  | -- | a query hash set associated to a batched request
    -- note that this does not need to contain multiple query hashes: it is possible
    -- for a batch to contain only one request
    PQHSetBatched ![ParameterizedQueryHash]
  deriving (Show, Eq)

-- | we use something that explicitly produces an 'J.Object' instead of writing
-- a 'J.ToJSON' instance. in the latter case, functions consuming the output of
-- 'J.toJSON' would have to perform a partial pattern-match on the 'J.Value'
-- output to extract a JSON object from it. for the other patterns, it would
-- have to either throw a runtime error on or silently ignore the other
-- patterns, and the latter choice would cause a silent failure if the
-- 'J.ToJSON' instance were modified to no longer always return objects
parameterizedQueryHashListToObject :: ParameterizedQueryHashList -> J.Object
parameterizedQueryHashListToObject =
  KM.fromList . \case
    -- when a non-graphql query is executed, or when the request fails,
    -- there are no hashes to log
    PQHSetEmpty -> []
    -- when there's no batching of graphql queries, we log the parameterized query hash as a string
    PQHSetSingleton queryHash ->
      [("parameterized_query_hash", J.toJSON queryHash)]
    -- when there's a batch of graphql queries (even if the batch contains only one request),
    -- we log the parameterized query hashes of every request in a list
    PQHSetBatched queryHashes ->
      [("parameterized_query_hash", J.toJSON queryHashes)]

newtype ParameterizedQueryHash = ParameterizedQueryHash {unParamQueryHash :: B.ByteString}
  deriving (Show, Eq, Ord)

instance Hashable ParameterizedQueryHash where
  hashWithSalt salt = hashWithSalt salt . unParamQueryHash

instance J.ToJSON ParameterizedQueryHash where
  toJSON = J.String . bsToTxt . unParamQueryHash

normalizeSelectionSet :: G.SelectionSet G.NoFragments Variable -> G.SelectionSet G.NoFragments Void
normalizeSelectionSet = (normalizeSelection =<<)
  where
    normalizeSelection :: G.Selection G.NoFragments Variable -> G.SelectionSet G.NoFragments Void
    normalizeSelection (G.SelectionField fld) = pure $ G.SelectionField (normalizeField fld)
    normalizeSelection (G.SelectionInlineFragment (G.InlineFragment _ _ selSet)) =
      normalizeSelectionSet selSet

    normalizeField (G.Field _alias name args _directives selSet) =
      G.Field Nothing name (HashMap.map normalizeValue args) mempty $ normalizeSelectionSet selSet

    normalizeConstValue :: G.Value Void -> G.Value Void
    normalizeConstValue = \case
      G.VNull -> G.VNull
      G.VInt _ -> G.VNull
      G.VFloat _ -> G.VNull
      G.VString _ -> G.VNull
      G.VBoolean _ -> G.VNull
      G.VEnum _ -> G.VNull
      G.VList l -> G.VList $ map normalizeConstValue l
      G.VObject obj -> G.VObject $ HashMap.map normalizeConstValue obj

    jsonToNormalizedGQLVal :: J.Value -> G.Value Void
    jsonToNormalizedGQLVal = \case
      J.Null -> G.VNull
      J.Bool _ -> G.VNull
      J.String _ -> G.VNull
      J.Number _ -> G.VNull
      J.Array l -> G.VList $ jsonToNormalizedGQLVal <$> toList l
      J.Object vals ->
        G.VObject
          $
          -- FIXME(#3479): THIS WILL CREATE INVALID GRAPHQL OBJECTS
          HashMap.fromList
            [ (name, jsonToNormalizedGQLVal val)
              | (key, val) <- KM.toList vals,
                name <- maybeToList (G.mkName (K.toText key))
            ]

    normalizeValue :: G.Value Variable -> G.Value Void
    normalizeValue = \case
      G.VNull -> G.VNull
      G.VInt _ -> G.VNull
      G.VFloat _ -> G.VNull
      G.VString _ -> G.VNull
      G.VBoolean _ -> G.VNull
      G.VEnum _ -> G.VNull
      G.VList l -> G.VList $ map normalizeValue l
      G.VObject obj -> G.VObject $ HashMap.map normalizeValue obj
      -- Pretend that variables without values are just nulls.
      G.VVariable (Variable _info _type Nothing) -> G.VNull
      G.VVariable (Variable _info _type (Just value)) ->
        case value of
          GraphQLValue val -> normalizeConstValue val
          JSONValue v -> jsonToNormalizedGQLVal v

calculateParameterizedQueryHash :: G.SelectionSet G.NoFragments Variable -> ParameterizedQueryHash
calculateParameterizedQueryHash = ParameterizedQueryHash . cryptoHash . Text.run . G.selectionSet . normalizeSelectionSet

mkUnsafeParameterizedQueryHash :: Text -> ParameterizedQueryHash
mkUnsafeParameterizedQueryHash = ParameterizedQueryHash . txtToBs
