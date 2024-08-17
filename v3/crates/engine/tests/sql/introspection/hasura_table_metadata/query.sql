SELECT
    *
FROM
    hasura.table_metadata
    JOIN hasura.column_metadata USING (schema_name, table_name);