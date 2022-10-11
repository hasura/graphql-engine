CREATE OR REPLACE FUNCTION hdb_catalog.check_violation(msg text) RETURNS bool AS
$$
  BEGIN
    RAISE check_violation USING message=msg;
  END;
$$ LANGUAGE plpgsql;
