ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT event_triggers_pkey;

ALTER TABLE hdb_catalog.event_triggers
  DROP CONSTRAINT event_triggers_name_key;

ALTER TABLE hdb_catalog.event_triggers
  ADD PRIMARY KEY (name);

ALTER TABLE hdb_catalog.event_triggers
  DROP COLUMN id;

ALTER TABLE hdb_catalog.event_log
  DROP COLUMN trigger_id;

CREATE INDEX ON hdb_catalog.event_log (trigger_name);

UPDATE hdb_catalog.hdb_relationship
   SET rel_def = '{"manual_configuration":{"remote_table":{"schema":"hdb_catalog","name":"event_log"},"column_mapping":{"name":"trigger_name"}}}'
 WHERE table_schema = 'hdb_catalog'
       AND table_name = 'event_triggers'
       AND rel_name = 'events';

UPDATE hdb_catalog.hdb_relationship
   SET rel_def = '{"manual_configuration":{"remote_table":{"schema":"hdb_catalog","name":"event_triggers"},"column_mapping":{"trigger_name":"name"}}}'
 WHERE table_schema = 'hdb_catalog'
       AND table_name = 'event_log'
       AND rel_name = 'trigger';
