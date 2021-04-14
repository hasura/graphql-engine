-- SCHEMA_NAME(..)
SELECT object.name, object.schema_id, object.object_id, object.type_desc,
       JSON_QUERY([schema].json) AS [joined_sys_schema],
       JSON_QUERY([column].json) AS [joined_sys_column]
FROM sys.objects object
       CROSS APPLY (SELECT [column].name, [column].column_id, [column].is_nullable, [column].user_type_id,
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
                                 FROM sys.foreign_key_columns [fk],
                                      sys.objects AS referenced_table,
                                      sys.columns AS referenced_column
                                 CROSS APPLY (SELECT [schema].name, [schema].schema_id
                                              FROM sys.schemas [schema]
                                              WHERE [schema].schema_id = object.schema_id
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
WHERE object.type_desc IN ('USER_TABLE', 'VIEW')
FOR JSON PATH
