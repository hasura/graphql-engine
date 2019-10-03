CREATE TABLE hdb_catalog.remote_schemas (
  id BIGSERIAL PRIMARY KEY,
  name TEXT UNIQUE,
  definition JSON,
  comment TEXT
);
