CREATE OR REPLACE VIEW hdb_catalog.hdb_primary_key AS
SELECT
  nr.nspname as table_schema,
  r.relname as table_name,
  c.conname as constraint_name,
  json_agg(a.attname) as "columns"
FROM
  pg_namespace nr,
  pg_class r,
  pg_attribute a,
  pg_namespace nc,
  pg_constraint c
WHERE
  nr.oid = r.relnamespace
  AND (r.oid = a.attrelid)
  AND (nc.oid = c.connamespace)
  -- only primary key constraint
  AND (c.contype = 'p')
  AND (r.oid = c.conrelid)
  AND (a.attnum = ANY (c.conkey))
  AND (NOT a.attisdropped)
  -- 'normal table or partitioned table'
  AND (r.relkind = 'r' OR r.relkind = 'p')
GROUP BY (nr.nspname, r.relname, c.conname);
