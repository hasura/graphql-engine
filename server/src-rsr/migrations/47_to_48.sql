-- Incorrect serialization of the metadata defaults in the metadata table is removed in this migration.

-- This helper function is required since the jsonb '-' operator only became available in pg15.
CREATE OR REPLACE FUNCTION hdb_catalog.remove_json_key__(
  IN object jsonb, 
  IN key_to_delete text, 
  OUT jsonb
)
  IMMUTABLE
  STRICT
  LANGUAGE SQL
AS 
$$
  SELECT jsonb_object_agg(key, value)
     FROM (SELECT key, value 
           FROM jsonb_each(object)
           WHERE
            key <> key_to_delete
     ) each_subselect
$$
;

-- One issue with this could be that a user may have already set a backend config in their metadata.
-- To prevent this we only remove the backend_config if it already matches the given athena value.
UPDATE hdb_catalog.hdb_metadata
  SET metadata = hdb_catalog.remove_json_key__(
    metadata :: jsonb,
    'backend_configs' :: text
  )
  WHERE
    ((metadata #> '{backend_configs,dataconnector}') :: jsonb)
    =
    ('{"athena": {"uri": "http://localhost:8081/api/v1/athena"}}' :: jsonb)
  ;

DROP FUNCTION IF EXISTS hdb_catalog.remove_json_key__(
  IN object jsonb, 
  IN key_to_delete text, 
  OUT jsonb
);
