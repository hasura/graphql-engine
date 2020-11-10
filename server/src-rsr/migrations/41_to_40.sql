ALTER TABLE hdb_catalog.event_log ALTER COLUMN locked TYPE BOOLEAN USING locked IS NOT NULL;
ALTER TABLE hdb_catalog.event_log ALTER COLUMN locked SET NOT NULL;
ALTER TABLE hdb_catalog.event_log ALTER COLUMN locked SET DEFAULT false;
