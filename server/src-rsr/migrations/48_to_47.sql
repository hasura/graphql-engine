-- This backwards migration is empty due to 47 to 48 smiply fixing a piece of data in metadata.

DROP FUNCTION IF EXISTS hdb_catalog.remove_json_key__(
  IN object jsonb, 
  IN key_to_delete text, 
  OUT jsonb
);
