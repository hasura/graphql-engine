ALTER TABLE hdb_catalog.hdb_relationship
  ADD COLUMN IF NOT EXISTS comment TEXT NULL;

ALTER TABLE hdb_catalog.hdb_permission
  ADD COLUMN IF NOT EXISTS comment TEXT NULL;

ALTER TABLE hdb_catalog.hdb_query_template
  ADD COLUMN IF NOT EXISTS comment TEXT NULL;

UPDATE hdb_catalog.hdb_query_template
    SET template_defn = json_build_object('type', 'select', 'args', template_defn->'select');
