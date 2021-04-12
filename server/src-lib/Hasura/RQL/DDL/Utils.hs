module Hasura.RQL.DDL.Utils
  ( clearHdbViews
  ) where

import qualified Database.PG.Query as Q

import           Hasura.Prelude

clearHdbViews :: Q.Tx ()
clearHdbViews = Q.multiQ (Q.fromText clearHdbViewsFunc)

clearHdbViewsFunc :: Text
clearHdbViewsFunc =
  "DO $$ DECLARE \
   \ r RECORD; \
   \ BEGIN \
   \   FOR r IN (SELECT routine_name FROM information_schema.routines WHERE specific_schema = 'hdb_views' ORDER BY routine_name) LOOP \
   \     EXECUTE 'DROP FUNCTION hdb_views.' || quote_ident(r.routine_name) || '() CASCADE'; \
   \   END LOOP; \
   \ END $$; "
