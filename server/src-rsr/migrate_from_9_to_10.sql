CREATE TABLE hdb_catalog.hdb_cache_update_event (
  id BIGSERIAL PRIMARY KEY,
  server_id uuid NOT NULL,
  occurred_at timestamptz NOT NULL
);

CREATE FUNCTION hdb_catalog.hdb_cache_update_event_notifier() RETURNS trigger AS
$function$
  DECLARE
    server_id uuid;
    occurred_at timestamptz;
    curr_rec record;
  BEGIN
    server_id = NEW.server_id;
    occurred_at = NEW.occurred_at;
    PERFORM pg_notify('hasura_cache_update', json_build_object(
      'server_id', server_id,
      'occurred_at', occurred_at
    )::text);
    RETURN curr_rec;
  END;
$function$
LANGUAGE plpgsql;

CREATE TRIGGER hdb_cache_update_event_notifier AFTER INSERT ON hdb_catalog.hdb_cache_update_event
  FOR EACH ROW EXECUTE PROCEDURE hdb_catalog.hdb_cache_update_event_notifier();
