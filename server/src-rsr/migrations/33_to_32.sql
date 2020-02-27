-- Nothing to do, since we can't recreate the old views.
-- Older server processes will create them on startup if needed 
-- anyway.
DO language plpgsql $$
BEGIN
  RAISE NOTICE 'Nothing to do';
END
$$;