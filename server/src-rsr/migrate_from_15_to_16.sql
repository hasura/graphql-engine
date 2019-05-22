CREATE TABLE hdb_catalog.hdb_query_collection
(
  collection_name TEXT PRIMARY KEY,
  collection_defn JSONB NOT NULL,
  comment TEXT NULL,
  is_system_defined boolean default false
);

CREATE TABLE hdb_catalog.hdb_allowlist
(
  collection_name TEXT UNIQUE
    REFERENCES hdb_catalog.hdb_query_collection(collection_name)
);
