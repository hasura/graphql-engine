This note is in [Hasura.Backends.MSSQL.DDL.EventTrigger](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/MSSQL/DDL/EventTrigger.hs#L1361).
It is referenced at:
  - line 456 of [Hasura.Backends.MSSQL.DDL.EventTrigger](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/MSSQL/DDL/EventTrigger.hs#L456)
  - line 1321 of [Hasura.Backends.MSSQL.DDL.EventTrigger](https://github.com/hasura/graphql-engine/blob/master/server/src-lib/Hasura/Backends/MSSQL/DDL/EventTrigger.hs#L1321)

# Encode Event Trigger Payload to JSON in SQL Server

We do not have JSON datatype in SQL Server. But since in 'mkAllTriggersQ' we
ensure that all the values in the payload column of hdb_catalog.event_log is
always a JSON. We can directly decode the payload value and not worry that the
decoding will fail.

We ensure that the values in 'hd_catalog.event_log' is always a JSON is by using
the 'FOR JSON PATH' MSSQL operand when inserting value into the
'hdb_catalog.event_log' table.


