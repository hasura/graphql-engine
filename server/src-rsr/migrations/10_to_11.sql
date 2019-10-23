CREATE TABLE hdb_catalog.hdb_schema_update_event (
  id BIGSERIAL PRIMARY KEY,
  instance_id uuid NOT NULL,
  occurred_at timestamptz NOT NULL DEFAULT NOW()
);

CREATE FUNCTION hdb_catalog.hdb_schema_update_event_notifier() RETURNS trigger AS
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

CREATE TRIGGER hdb_schema_update_event_notifier AFTER INSERT ON hdb_catalog.hdb_schema_update_event
  FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.hdb_schema_update_event_notifier();
