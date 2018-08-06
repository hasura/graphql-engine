{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Hasura.RQL.DDL.Utils where

import qualified Database.PG.Query as Q

import           Hasura.Prelude
import           Hasura.RQL.Types

removeAllViewsQ :: Q.Query
removeAllViewsQ =
  "DO $$ DECLARE \
   \ r RECORD; \
   \ BEGIN \
   \   FOR r IN (SELECT viewname FROM pg_views WHERE schemaname = 'hdb_views') LOOP \
   \     EXECUTE 'DROP VIEW IF EXISTS hdb_views.' || quote_ident(r.viewname) || ' CASCADE'; \
   \   END LOOP; \
   \ END $$ "

cleanHdbViews :: Q.TxE QErr ()
cleanHdbViews = Q.catchE defaultTxErrorHandler $ do
  Q.unitQ removeAllViewsQ () False
  initDefaultViews

initDefaultViews :: Q.Tx ()
initDefaultViews = do
  Q.Discard () <- Q.multiQ $(Q.sqlFromFile "src-rsr/init_views.sql")
  return ()
