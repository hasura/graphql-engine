This note is in [Hasura.Backends.MSSQL.DDL.EventTrigger](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/MSSQL/DDL/EventTrigger.hs#L781).
It is referenced at:
  - line 839 of [Hasura.Backends.MSSQL.DDL.EventTrigger](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/MSSQL/DDL/EventTrigger.hs#L839)

# Update Event Trigger MSSQL Spec

An MS-SQL trigger is different from a postgres trigger in some ways
  * MS-SQL doesn't support triggers which trigger for each row, so in case of
    mutations which affect multiple rows, there'll only be a single trigger
    fired which will contain the data of all the rows that were affected.
  * MS-SQL maintains two logical tables, namely, [`inserted` and `deleted`](https://docs.microsoft.com/en-us/sql/relational-databases/triggers/use-the-inserted-and-deleted-tables?view=sql-server-ver15).
    The rows in the `inserted` table are copies of the new rows in the trigger
    table and similarly the `deleted` table contains the copies of the rows
    that were deleted from the trigger table.
  * When there's an update transaction, the old data (before the update) will
    be copied to the `deleted` table and the new data will be copied to the
    `inserted` table.

Since we deliver the 'old' and 'new' data in the event trigger payload, we would need
a way to correlate the values from `inserted` and `deleted` tables. And this is why,
It is mandatory for a MSSQL Update trigger table to have a primary key. We use this
primary key to correlate between `inserted` and `deleted`

MSSQL UPDATE trigger's join clause depends on the fact that the primary key is never
updated. But when the primary key is updated, you cannot join the 'INSERTED' and
the 'DELETED' tables. Hence for those cases, we consider the old payload as NULL and
the new payload will contain the updated row changes.

To figure out if a primary key has been updated, we do the following:
For each row present in the INSERTED table, we check if there are any rows in DELETED
tabled that has the same primary key as that of the row in INSERTED table. If such a
row does not exists, then it means that the primary key has been updated. The sample
SQL which does this looks like:
  SELECT * FROM INSERTED
  WHERE NOT EXISTS (SELECT * FROM DELETED WHERE  INSERTED.id = DELETED.id )

The spec for MSSQL UPDATE Event Trigger is as follows:
1. UPDATE Event Trigger can only be created on tables with a primary key.
2. When Primary Key is not updated during a UPDATE transaction then both 'data.new'
   and 'data.old' fields in payload will be constructed.
3. When Primary Key is updated during a UPDATE transaction then there are two cases:
    a. If the updated Primary key is equal to one of the already present primary key in
       the table then, we construct both the 'data.old' and 'data.new'
    b. If the updated primary key is not equal to any of the already present primary key
       in the table then, 'data.old' is NULL and only 'data.new' is constructed.

