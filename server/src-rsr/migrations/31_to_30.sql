TRUNCATE hdb_catalog.hdb_schema_update_event;
CREATE OR REPLACE FUNCTION hdb_catalog.hdb_schema_update_event_notifier() RETURNS trigger AS
$function$
  DECLARE
    instance_id uuid;
    occurred_at timestamptz;
    curr_rec record;
  BEGIN
    instance_id = NEW.instance_id;
    occurred_at = NEW.occurred_at;
    PERFORM pg_notify('hasura_schema_update', json_build_object(
      'instance_id', instance_id,
      'occurred_at', occurred_at
      )::text);
    RETURN curr_rec;
  END;
$function$
LANGUAGE plpgsql;
ALTER TABLE hdb_catalog.hdb_schema_update_event DROP COLUMN invalidations;
