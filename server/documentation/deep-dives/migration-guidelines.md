# Guidelines for adding migrations

## 1. Additive changes should be preferred over transformational changes.

   For example,

   We have to change a type of the column `locked` from `bool` to `timestamp with time zone`
   where when the value of the column is `true` then the new value should be the
   current timestamp and when the value of the column is `false` it should be `null`.

   Instead of doing,

   ```sql
   ALTER TABLE hdb_catalog.event_log ALTER COLUMN locked TYPE TIMESTAMPTZ USING CASE WHEN locked THEN NOW() ELSE NULL END;
   ```

   It's preferred to do,

   ```sql
    ALTER TABLE hdb_catalog.event_log RENAME COLUMN locked TO locked_boolean;

    ALTER TABLE hdb_catalog.event_log ADD COLUMN locked TIMESTAMPTZ;

    UPDATE hdb_catalog.event_log
    SET locked = NOW()
    WHERE locked_boolean = 't';

    ALTER TABLE hdb_catalog.event_log DROP COLUMN locked_boolean;
   ```

   This is because in the former way, the `ALTER TABLE` will acquire an lock on the `hdb_catalog.event_log` table
   and will keep the lock until all the rows are transformed to the new data type. The latter method is preffered although
   it does 4 queries, because all the DDL statements do not depend on the data contained in the rows.

## 2. Avoid adding a default value while adding a new column to an existing table

      Instead of doing,

      ```sql
      ALTER TABLE <table>
      ADD COLUMN col1 INT SET DEFAULT 1 NOT NULL;
      ```

      Do,

      ```sql
      ALTER TABLE <table>
      ADD COLUMN col1 INT;

      UPDATE <table>
      SET col1 = 1;

      ALTER TABLE <table>
        ALTER COLUMN col1 SET DEFAULT 1
        ALTER COLUMN col1 SET NOT NULL;
      ```
