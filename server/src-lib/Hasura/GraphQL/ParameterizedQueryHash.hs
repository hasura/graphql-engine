{-|
This module calculates parameterized query hash, which is a way to
hash an incoming query (after resolving variables) with all leaf nodes
(i.e. scalar values) discarded. In other words, two queries having the same
parameterized query hash are essentially the same query but may differ in
leaf values.

For example:

1. query {
     authors (where: {id: {_eq: 2}}) {
       id
       name
     }
   }

2. query {
     authors (where: {id: {_eq: 203943}}) {
       id
       name
     }
   }

3. query {
     authors (where: {id: {_eq: $id}}) {
       id
       name
     }
   }

  For any value of `id`

4. query {
     authors (where: $whereBoolExp) {
       id
       name
     }
   }

   only when `whereBoolExp` is of the form of

   {
      "id": {
        "_eq": <id>
      }
   }

All the above queries should result in the same parameterized query hash.

The following steps are done to calculate the parameterized query hash:

1. Normalize the GraphQL query by substituting the variables (if any) in appropriate places.
2. Substitute any scalar GraphQL values (Int, Float, Enum, String and Boolean) to null
3. For input objects and list, traverse through them and do step no 2.
4. Calculate the hash of the query obtained from step 3.

Note: Parameterized query hash is a PRO only feature
-}

module Hasura.GraphQL.ParameterizedQueryHash
  ( calculateParameterizedQueryHash
  , ParameterizedQueryHash
  )
where

import           Hasura.Prelude

import qualified Data.Aeson                     as J
import qualified Data.ByteString                as B
import qualified Data.HashMap.Strict            as Map
import qualified Language.GraphQL.Draft.Printer as G
import qualified Language.GraphQL.Draft.Syntax  as G
import qualified Text.Builder                   as Text

import           Hasura.GraphQL.Parser          (InputValue (..), Variable (..))

import           Hasura.Server.Utils            (cryptoHash)

newtype ParameterizedQueryHash
  = ParameterizedQueryHash { unParamQueryHash :: B.ByteString }
  deriving (Show, Eq)

instance J.ToJSON ParameterizedQueryHash where
  toJSON = J.String  . bsToTxt . unParamQueryHash

normalizeSelectionSet :: G.SelectionSet G.NoFragments Variable -> G.SelectionSet G.NoFragments Void
normalizeSelectionSet =  (normalizeSelection =<<)
  where
    normalizeSelection :: G.Selection G.NoFragments Variable -> G.SelectionSet G.NoFragments Void
    normalizeSelection (G.SelectionField fld) = pure $ G.SelectionField (normalizeField fld)
    normalizeSelection (G.SelectionInlineFragment (G.InlineFragment _ _ selSet)) =
      normalizeSelectionSet selSet

    normalizeField (G.Field _alias name args _directives selSet) =
      G.Field Nothing name (Map.map normalizeValue args) mempty $ normalizeSelectionSet selSet

    normalizeConstValue :: G.Value Void -> G.Value Void
    normalizeConstValue = \case
      G.VNull       -> G.VNull
      G.VInt _      -> G.VNull
      G.VFloat _    -> G.VNull
      G.VString _   -> G.VNull
      G.VBoolean _  -> G.VNull
      G.VEnum _     -> G.VNull
      G.VList l     -> G.VList $ map normalizeConstValue l
      G.VObject obj -> G.VObject $ Map.map normalizeConstValue obj

    jsonToNormalizedGQLVal :: J.Value -> G.Value Void
    jsonToNormalizedGQLVal = \case
      J.Null -> G.VNull
      J.Bool _ -> G.VNull
      J.String _ -> G.VNull
      J.Number _ -> G.VNull
      J.Array l  -> G.VList $ jsonToNormalizedGQLVal <$> toList l
      J.Object vals -> G.VObject $ Map.fromList $
        flip map (Map.toList vals) $ \(key, val) ->
          (G.unsafeMkName key, jsonToNormalizedGQLVal val)

    normalizeValue :: G.Value Variable -> G.Value Void
    normalizeValue = \case
      G.VNull                 -> G.VNull
      G.VInt _                -> G.VNull
      G.VFloat _              -> G.VNull
      G.VString _             -> G.VNull
      G.VBoolean _            -> G.VNull
      G.VEnum _               -> G.VNull
      G.VList l     -> G.VList $ map normalizeValue l
      G.VObject obj -> G.VObject $ Map.map normalizeValue obj
      G.VVariable (Variable _info _type value) ->
        case value of
          GraphQLValue val -> normalizeConstValue val
          JSONValue v      -> jsonToNormalizedGQLVal v

calculateParameterizedQueryHash :: G.SelectionSet G.NoFragments Variable -> ParameterizedQueryHash
calculateParameterizedQueryHash = ParameterizedQueryHash . cryptoHash . Text.run . G.selectionSet . normalizeSelectionSet
