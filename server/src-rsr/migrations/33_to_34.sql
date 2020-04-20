DO $$ DECLARE
  r RECORD;
  BEGIN
    FOR r IN (SELECT viewname FROM pg_views WHERE schemaname = 'hdb_views' ORDER BY viewname) LOOP
      EXECUTE 'DROP VIEW IF EXISTS hdb_views.' || quote_ident(r.viewname) || ' CASCADE';
    END LOOP;
  END $$;
