-- Create a function that always returns the first non-NULL item
CREATE OR REPLACE FUNCTION hdb_catalog.first_agg ( anyelement, anyelement )
RETURNS anyelement LANGUAGE SQL IMMUTABLE STRICT AS $$
       SELECT $1;
$$;

-- And then wrap an aggregate around it
CREATE AGGREGATE hdb_catalog.FIRST (
       sfunc    = hdb_catalog.first_agg,
       basetype = anyelement,
       stype    = anyelement
);

-- Create a function that always returns the last non-NULL item
CREATE OR REPLACE FUNCTION hdb_catalog.last_agg ( anyelement, anyelement )
RETURNS anyelement LANGUAGE SQL IMMUTABLE STRICT AS $$
        SELECT $2;
$$;

-- And then wrap an aggregate around it
CREATE AGGREGATE hdb_catalog.LAST (
       sfunc    = hdb_catalog.last_agg,
       basetype = anyelement,
       stype    = anyelement
);
