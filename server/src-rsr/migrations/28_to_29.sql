CREATE OR REPLACE FUNCTION hdb_catalog.check_violation() RETURNS bool AS 
$$
  BEGIN
    RAISE check_violation USING message='insert check constraint failed';
  END;
$$ LANGUAGE plpgsql;