TRUNCATE hdb_catalog.hdb_schema_update_event;
ALTER TABLE hdb_catalog.hdb_schema_update_event ADD COLUMN invalidations json NOT NULL;
CREATE OR REPLACE FUNCTION hdb_catalog.hdb_schema_update_event_notifier() RETURNS trigger AS
$function$
  DECLARE
    instance_id uuid;
    occurred_at timestamptz;
    invalidations json;
    curr_rec record;
  BEGIN
    instance_id = NEW.instance_id;
    occurred_at = NEW.occurred_at;
    invalidations = NEW.invalidations;
    PERFORM pg_notify('hasura_schema_update', json_build_object(
      'instance_id', instance_id,
      'occurred_at', occurred_at,
      'invalidations', invalidations
      )::text);
    RETURN curr_rec;
  END;
$function$
LANGUAGE plpgsql;
