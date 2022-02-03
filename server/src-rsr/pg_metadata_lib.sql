CREATE SCHEMA IF NOT EXISTS hdb_lib;

CREATE OR REPLACE FUNCTION
  hdb_lib.pg_attidentity()
  RETURNS TABLE (attrelid oid, attname name, attnum smallint, attidentity char) AS $$
    BEGIN
      IF current_setting('server_version_num')::int >= 100000
      THEN RETURN QUERY
        SELECT a.attrelid, a.attname, a.attnum, a.attidentity::char
        FROM pg_catalog.pg_attribute a;
      ELSE
        -- Always return attidentity = '', indicating that the column is not an
        -- identity column.
        RETURN QUERY
        SELECT a.attrelid, a.attname, a.attnum, ''::char as attidentity
        FROM pg_catalog.pg_attribute a;
    END IF;
  END;
$$ LANGUAGE plpgsql;
COMMENT ON FUNCTION hdb_lib.pg_attidentity() IS
'The column "pg_catalog.pg_attribute(attidentity)" was only introduced in PG 10,
along with with the introduction of identity columns.
This function provides the "attidentity" column in a cross-version compatible way.
See https://www.postgresql.org/docs/10/catalog-pg-attribute.html for details.
';

CREATE OR REPLACE FUNCTION
  hdb_lib.pg_attgenerated()
  RETURNS TABLE (attrelid oid, attname name, attnum smallint, attgenerated char) AS $$
    BEGIN
      IF current_setting('server_version_num')::int >= 120000
      THEN RETURN QUERY
        SELECT a.attrelid, a.attname, a.attnum, a.attgenerated::char
        FROM pg_catalog.pg_attribute a;
      ELSE
        -- Always return attgenerated = '', indicating that the column is not a
        -- generated column.
        RETURN QUERY
        SELECT a.attrelid, a.attname, a.attnum, ''::char as attgenerated
        FROM pg_catalog.pg_attribute a;
    END IF;
  END;
$$ LANGUAGE plpgsql;
COMMENT ON FUNCTION hdb_lib.pg_attgenerated() IS
'The column "pg_catalog.pg_attribute(attgenerated)" was only introduced in PG 12,
along with the introduction of generated columns.
This function provides the "attgenerated" column in a cross-version compatible way.
See https://www.postgresql.org/docs/12/catalog-pg-attribute.html for details.
';
