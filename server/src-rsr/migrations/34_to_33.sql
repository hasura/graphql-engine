DO $$ DECLARE
  r RECORD;
  BEGIN
    FOR r IN (SELECT viewname FROM pg_views WHERE schemaname = 'hdb_views' ORDER BY viewname) LOOP
      EXECUTE 'DROP VIEW IF EXISTS hdb_views.' || quote_ident(r.viewname) || ' CASCADE';
    END LOOP;
  END $$;

DO $$ DECLARE
  r RECORD;
  BEGIN
    FOR r IN (SELECT routine_name FROM information_schema.routines WHERE specific_schema = 'hdb_views' ORDER BY routine_name) LOOP
      EXECUTE 'DROP FUNCTION hdb_views.' || quote_ident(r.routine_name) || '() CASCADE';
    END LOOP;
  END $$;
