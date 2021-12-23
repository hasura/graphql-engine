ALTER TABLE hdb_catalog.event_log
ALTER COLUMN id
SET DEFAULT hdb_catalog.gen_hasura_uuid();

ALTER TABLE hdb_catalog.event_invocation_logs
ALTER COLUMN id
SET DEFAULT hdb_catalog.gen_hasura_uuid();

ALTER TABLE hdb_catalog.event_log RENAME COLUMN locked TO locked_boolean;

ALTER TABLE hdb_catalog.event_log ADD COLUMN locked TIMESTAMPTZ;

UPDATE hdb_catalog.event_log
SET locked = NOW()
WHERE locked_boolean = 't';

ALTER TABLE hdb_catalog.event_log DROP COLUMN locked_boolean;
