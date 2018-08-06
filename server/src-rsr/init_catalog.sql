CREATE TABLE hdb_catalog.hdb_version (
    version TEXT NOT NULL,
    upgraded_on TIMESTAMPTZ NOT NULL
);

CREATE UNIQUE INDEX hdb_version_one_row
ON hdb_catalog.hdb_version((version IS NOT NULL));

CREATE TABLE hdb_catalog.hdb_table
(
    table_schema TEXT,
    table_name TEXT,
    is_system_defined boolean default false,

    PRIMARY KEY (table_schema, table_name)
);

CREATE FUNCTION hdb_catalog.hdb_table_oid_check() RETURNS trigger AS
$function$
  BEGIN
    IF (EXISTS (SELECT 1 FROM information_schema.tables st WHERE st.table_schema = NEW.table_schema AND st.table_name = NEW.table_name)) THEN
      return NEW;
    ELSE
      RAISE foreign_key_violation using message = 'table_schema, table_name not in information_schema.tables';
      return NULL;
    END IF;
  END;
$function$
LANGUAGE plpgsql;

CREATE TRIGGER hdb_table_oid_check BEFORE INSERT OR UPDATE ON hdb_catalog.hdb_table
       FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.hdb_table_oid_check();

CREATE TABLE hdb_catalog.hdb_relationship
(
    table_schema TEXT,
    table_name TEXT,
    rel_name   TEXT,
    rel_type   TEXT CHECK (rel_type IN ('object', 'array')),
    rel_def    JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false,

    PRIMARY KEY (table_schema, table_name, rel_name),
    FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name)
);

CREATE TABLE hdb_catalog.hdb_permission
(
    table_schema TEXT,
    table_name TEXT,
    role_name  TEXT,
    perm_type  TEXT CHECK(perm_type IN ('insert', 'select', 'update', 'delete')),
    perm_def   JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false,

    PRIMARY KEY (table_schema, table_name, role_name, perm_type),
    FOREIGN KEY (table_schema, table_name) REFERENCES hdb_catalog.hdb_table(table_schema, table_name)
);

CREATE TABLE hdb_catalog.hdb_query_template
(
    template_name TEXT PRIMARY KEY,
    template_defn JSONB NOT NULL,
    comment    TEXT NULL,
    is_system_defined boolean default false
);

CREATE FUNCTION hdb_catalog.inject_table_defaults(view_schema text, view_name text, tab_schema text, tab_name text) RETURNS void
LANGUAGE plpgsql AS $$
    DECLARE
        r RECORD;
    BEGIN
      FOR r IN SELECT column_name, column_default FROM information_schema.columns WHERE table_schema = tab_schema AND table_name = tab_name AND column_default IS NOT NULL LOOP
          EXECUTE format('ALTER VIEW %I.%I ALTER COLUMN %I SET DEFAULT %s;', view_schema, view_name, r.column_name, r.column_default);
      END LOOP;
    END;
$$;
