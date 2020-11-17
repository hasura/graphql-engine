module Hasura.GraphQL.Execute.Join where

import           Hasura.Prelude

import           Control.Arrow       (left)
import           Data.Foldable

import qualified Data.Aeson          as J
import qualified Data.Aeson.Internal as J
import qualified Data.HashMap.Strict as HMS
import qualified Data.Text           as T

import           Hasura.EncJSON


-- | Given a path and a JSON object, extract all matching values.
--
-- This function assumes that arrays should not be filtered (as if the path had
-- been written using [*]). This slightly reinterprets the meaning of Aeson's
-- JSONPathElement (the index is ignored).
--
-- For a JSON path "data.articles[*].author" (which would be represented as
-- `[Key "data", Key "articles", Index _, Key "author"]`), and given a JSON object:
--   { "data":
--     { "articles: [
--         {"id": 0, "name": "foo", "author": "A.A"},
--         {"id": 1, "name": "bar", "author": "B.B"}
--       ]
--     }
--   }
--
-- this function would return `[J.String "A.A", J.String "B.B"]`.
extractPrimaryKeys :: J.JSONPath -> EncJSON -> Either Text [J.Value]
extractPrimaryKeys path response = do
  jsonValue <- left T.pack $ J.eitherDecode $ encJToLBS response
  foldlM walk [jsonValue] path
  where
    walk :: [J.Value] -> J.JSONPathElement -> Either Text [J.Value]
    walk values step = fmap concat $ for values \value -> case (value, step) of
      (J.Object object, J.Key key) -> case HMS.lookup key object of
        Nothing -> Left $ "key " <> key <> " not found"
        Just v  -> Right [v]
      (J.Array  array,  J.Index _) -> Right $ toList array
      _                            -> Left "failed to expand path" -- FIXME

-- | Given a path and a JSON object, create a map of all matching objects. Keys
-- | of the map will be the JSON value at the path, associated to the surrounding
-- | object.
--
-- This function assumes that arrays should not be filtered (as if the path had
-- been written using [*]). This slightly reinterprets the meaning of Aeson's
-- JSONPathElement (the index is ignored).
--
-- For a JSON path "data.authors.name" (which would be represented as
-- `[Key "data", Key "authors", Key "name"]`), and given a JSON object:
--   { "data":
--     { "authors: [
--         {"id": 0, "name": "A.A", "age": 42},
--         {"id": 1, "name": "B.B", "age": 64}
--       ]
--     }
--   }
--
-- this function would return a map:
--   "A.A" => {"id": 0, "name": "A.A", "age": 42}
--   "B.B" => {"id": 1, "name": "B.B", "age": 64}
extractObjects :: J.JSONPath -> EncJSON -> Either Text (HMS.HashMap J.Value J.Value)
extractObjects path response = do
  jsonValue <- left T.pack $ J.eitherDecode $ encJToLBS response
  assocList <- foldlM walk [(jsonValue, J.Null)] path
  Right $ HMS.fromList assocList
  where
    walk :: [(J.Value, J.Value)] -> J.JSONPathElement -> Either Text [(J.Value, J.Value)]
    walk values step = fmap concat $ for values \(value, _) -> case (value, step) of
      (J.Object object, J.Key key) -> case HMS.lookup key object of
        Nothing -> Left $ "key " <> key <> " not found"
        Just v  -> Right [(v, value)]
      (J.Array  array,  J.Index _) -> Right $ (,J.Null) <$> toList array
      _                            -> Left "failed to expand path" -- FIXME

-- | Given a path, a JSON object, and a mapping of JSON values, traverse the
-- | JSON object and replace elements at the given path by the one given in the
-- | map.
--
-- This function assumes that arrays should not be filtered (as if the path had
-- been written using [*]). This slightly reinterprets the meaning of Aeson's
-- JSONPathElement (the index is ignored).
--
-- *!* WARNING *!*
-- This function makes a MAJOR assumption, which is probably wrong: that there
-- WILL be a corresponding JSON value for every lookup we do.
--
-- Given:
--   the JSON path "data.articles[*].author"
--   the following mapping:
--     "A.A" => {"id": 0, "name": "A.A", "age": 42}
--     "B.B" => {"id": 1, "name": "B.B", "age": 64}
--   and the following JSON object
--     { "data":
--       { "articles: [
--           {"id": 0, "name": "foo", "author": "A.A"},
--           {"id": 1, "name": "bar", "author": "B.B"}
--         ]
--       }
--     }
-- This function would return:
--   { "data":
--     { "articles: [
--         {"id": 0, "name": "foo", "author": {
--             "id": 0, "name": "A.A", "age": 42
--           }
--         },
--         {"id": 1, "name": "bar", "author": {
--             "id": 1, "name": "B.B", "age": 64
--           }
--         }
--       ]
--     }
--   }
joinJSON :: J.JSONPath -> HMS.HashMap J.Value J.Value -> EncJSON -> Either Text EncJSON
joinJSON path objectMap query =
  fmap encJFromJValue $ replace path =<< left T.pack (J.eitherDecode $ encJToLBS query)
  where
    replace :: J.JSONPath -> J.Value -> Either Text J.Value
    replace [J.Key key] value = case value of
      J.Object object -> case HMS.lookup key object of
        Nothing -> Left $ "key " <> key <> " not found"
        Just pk -> case HMS.lookup pk objectMap of
          -- this assumes that all joins WILL have a matching object in the response!
          Nothing  -> Left $ "object with primary key " <> key <> " not found in sub query"
          Just val -> Right val
      _               -> Left "failed to expand path"
    replace (step : rest) value = case (value, step) of
      (J.Object object, J.Key key) -> do
        newValue <- case HMS.lookup key object of
          Nothing -> Left $ "key " <> key <> " not found"
          Just v  -> replace rest v
        Right $ J.Object $ HMS.insert key newValue object
      (J.Array  array,  J.Index _) -> J.Array <$> traverse (replace rest) array
      _                            -> Left "failed to expand path" -- FIXME
    replace _ _ = Left "failed to expand path" -- FIXME
