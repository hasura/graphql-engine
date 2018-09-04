{-# LANGUAGE OverloadedStrings #-}

module Hasura.RQL.DDL.Permission.Triggers where

import           Hasura.Prelude
import           Hasura.SQL.Types

import qualified Database.PG.Query       as Q
import qualified Hasura.SQL.DML          as S

import qualified Data.ByteString.Builder as BB

buildInsTrig :: QualifiedTable -> Q.Query
buildInsTrig qt@(QualifiedTable _ tn) =
  Q.fromBuilder $ mconcat
  [ BB.string7 "CREATE TRIGGER " <> toSQL tn
  , BB.string7 " INSTEAD OF INSERT ON " <> toSQL qt
  , BB.string7 " FOR EACH ROW EXECUTE PROCEDURE "
  , toSQL qt <> BB.string7 "();"
  ]

dropInsTrigFn :: QualifiedTable -> Q.Query
dropInsTrigFn fn =
  Q.fromBuilder $ BB.string7 "DROP FUNCTION " <> toSQL fn <> "()"

buildInsTrigFn :: QualifiedTable -> QualifiedTable -> S.BoolExp -> Q.Query
buildInsTrigFn fn tn be =
  Q.fromBuilder $ mconcat
  [ BB.string7 "CREATE OR REPLACE FUNCTION " <> toSQL fn
  , BB.string7 "() RETURNS trigger LANGUAGE plpgsql AS $$ "
  , BB.string7 "DECLARE r " <> toSQL tn <> "%ROWTYPE; "
  , BB.string7 "DECLARE conflict_clause jsonb; DECLARE action text; "
  , BB.string7 "DECLARE constraint_name text; "
  , BB.string7 "DECLARE set_expression text; "
  , BB.string7 "BEGIN "
  , BB.string7 "conflict_clause = current_setting('hasura.conflict_clause')::jsonb; "
  , BB.string7 "IF (" <> toSQL be <> BB.string7 ") THEN "
  , BB.string7 "CASE "
  , BB.string7 "WHEN conflict_clause = 'null'::jsonb THEN INSERT INTO " <> toSQL tn
  , BB.string7 " VALUES (NEW.*) RETURNING * INTO r; RETURN r; "
  , BB.string7 "ELSE "
  , BB.string7 "action = conflict_clause ->> 'action'; "
  , BB.string7 "constraint_name = conflict_clause ->> 'constraint'; "
  , BB.string7 "set_expression = conflict_clause ->> 'set_expression'; "
  , BB.string7 "IF action is NOT NULL THEN "
  , BB.string7 "CASE "
  , BB.string7 "WHEN action = 'ignore'::text AND constraint_name IS NULL THEN "
  , BB.string7 "INSERT INTO " <> toSQL tn
  , BB.string7 " VALUES (NEW.*) ON CONFLICT DO NOTHING RETURNING * INTO r; RETURN r; "
  , BB.string7 "WHEN action = 'ignore'::text AND constraint_name is NOT NULL THEN "
  , BB.string7 "EXECUTE 'INSERT INTO " <> toSQL tn
  , BB.string7 " VALUES ($1.*) ON CONFLICT ON CONSTRAINT ' || constraint_name || ' DO NOTHING RETURNING *' INTO r USING NEW; RETURN r; "
  , BB.string7 "ELSE EXECUTE 'INSERT INTO " <> toSQL tn
  , BB.string7 " VALUES ($1.*) ON CONFLICT ON CONSTRAINT ' || constraint_name || ' DO UPDATE ' || set_expression || "
  , BB.string7 "' RETURNING *' INTO r USING NEW; RETURN r; "
  , BB.string7 "END CASE; "
  , BB.string7 "ELSE RAISE internal_error using message = 'action is not found'; RETURN NULL; "
  , BB.string7 "END IF; "
  , BB.string7 "END CASE; "
  , BB.string7 "ELSE RAISE check_violation using message = 'insert check constraint failed'; RETURN NULL; "
  , BB.string7 "END IF; "
  , BB.string7 "END $$;"
  ]
