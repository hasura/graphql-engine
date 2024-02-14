-- SCHEMA_NAME(..)
SELECT ISNULL(
    (SELECT object.name, object.schema_id, object.object_id, object.type_desc,
           JSON_QUERY([schema].json) AS [joined_sys_schema],
           JSON_QUERY([column].json) AS [joined_sys_column],
           JSON_QUERY([primary_key].json) AS [joined_sys_primary_key]
    FROM sys.objects object
           CROSS APPLY (SELECT [column].name, [column].column_id, [column].is_nullable, [column].is_identity, [column].is_computed, [column].user_type_id,
                               JSON_QUERY([types].json) AS [joined_sys_type],
                               JSON_QUERY(ISNULL([relationships].json,'[]')) AS [joined_foreign_key_columns]
                        FROM sys.columns [column]
                        CROSS APPLY (SELECT name, schema_id, user_type_id FROM sys.types [type]
                                     WHERE [type].user_type_id = [column].user_type_id
                                     FOR JSON PATH, WITHOUT_ARRAY_WRAPPER)
                           AS [types](json)
                        CROSS APPLY (SELECT fk.*,
                                            referenced_table.name AS joined_referenced_table_name,
                                            referenced_column.name AS joined_referenced_column_name,
                                            JSON_QUERY([schema].json) AS [joined_referenced_sys_schema]
                                     FROM sys.foreign_key_columns AS [fk],
                                          sys.columns AS referenced_column,
                                          sys.objects AS referenced_table
                                     CROSS APPLY (SELECT [schema].name, [schema].schema_id
                                                  FROM sys.schemas [schema]
                                                  WHERE [schema].schema_id = [referenced_table].schema_id
                                                  FOR JSON PATH, WITHOUT_ARRAY_WRAPPER)
                                       AS [schema](json)
                                     WHERE [object].object_id = fk.parent_object_id
                                       AND [referenced_table].object_id = fk.referenced_object_id
                                       AND [referenced_column].object_id = [referenced_table].object_id
                                       AND [referenced_column].column_id = fk.referenced_column_id
                                       AND [column].column_id = fk.parent_column_id
                                     FOR JSON PATH)
                          AS [relationships](json)
                        WHERE [column].object_id = object.object_id
                        FOR JSON PATH)
             AS [column](json)
           CROSS APPLY (SELECT [schema].name, [schema].schema_id
                        FROM sys.schemas [schema]
                        WHERE [schema].schema_id = object.schema_id
                        FOR JSON PATH, WITHOUT_ARRAY_WRAPPER)
             AS [schema](json)
           CROSS APPLY (SELECT pk.name, pk.index_id, JSON_QUERY([cols].json) AS columns
                          FROM sys.indexes pk
                               CROSS APPLY (SELECT col.name
                                              FROM sys.index_columns ic
                                                   INNER JOIN sys.columns col
                                                       ON col.column_id = ic.column_id
                                                       AND col.object_id = ic.object_id
                                             WHERE ic.object_id = pk.object_id
                                               AND ic.index_id = pk.index_id
                                                   FOR JSON PATH)
                                 AS [cols](json)
                         WHERE pk.object_id = object.object_id
                           AND pk.is_primary_key = 1
                               FOR JSON PATH, WITHOUT_ARRAY_WRAPPER)
             AS [primary_key](json)
    WHERE object.type_desc IN ('USER_TABLE', 'VIEW')
    FOR JSON PATH)
    , '[]')
