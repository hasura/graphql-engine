-- | This module defines the type class 'PostgresAnnotatedFieldJSON'.
module Hasura.Backends.Postgres.Translate.Select.AnnotatedFieldJSON
  ( PostgresAnnotatedFieldJSON (..),
  )
where

import Data.Text qualified as T
import Hasura.Backends.Postgres.SQL.DML qualified as S
import Hasura.Backends.Postgres.Translate.Select.Internal.Helpers (withJsonBuildObj)
import Hasura.Prelude
import Hasura.RQL.Types.Common (FieldName (getFieldNameTxt))
import Hasura.SQL.Backend (PostgresKind (..))

class PostgresAnnotatedFieldJSON (pgKind :: PostgresKind) where
  annRowToJson :: FieldName -> [(FieldName, S.SQLExp)] -> (S.Alias, S.SQLExp)

instance PostgresAnnotatedFieldJSON 'Vanilla where
  annRowToJson fieldAlias fieldExps =
    -- postgres ignores anything beyond 63 chars for an iden
    -- in this case, we'll need to use json_build_object function
    -- json_build_object is slower than row_to_json hence it is only
    -- used when needed
    if any ((> 63) . T.length . getFieldNameTxt . fst) fieldExps
      then withJsonBuildObj fieldAlias $ concatMap toJsonBuildObjectExps fieldExps
      else withRowToJSON fieldAlias $ map toRowToJsonExtr fieldExps
    where
      toJsonBuildObjectExps (fieldName, fieldExp) =
        [S.SELit $ getFieldNameTxt fieldName, fieldExp]

      toRowToJsonExtr (fieldName, fieldExp) =
        S.Extractor fieldExp $ Just $ S.toAlias fieldName

      -- uses row_to_json to build a json object
      withRowToJSON ::
        FieldName -> [S.Extractor] -> (S.Alias, S.SQLExp)
      withRowToJSON parAls extrs =
        (S.toAlias parAls, jsonRow)
        where
          jsonRow = S.applyRowToJson extrs

instance PostgresAnnotatedFieldJSON 'Citus where
  annRowToJson fieldAlias fieldExps =
    -- Due to the restrictions Citus imposes on joins between tables of various
    -- distribution types we cannot use row_to_json and have to only rely on
    -- json_build_object.
    withJsonBuildObj fieldAlias $ concatMap toJsonBuildObjectExps fieldExps
    where
      toJsonBuildObjectExps (fieldName, fieldExp) =
        [S.SELit $ getFieldNameTxt fieldName, fieldExp]
