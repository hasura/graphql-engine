SELECT
  t.schema_name,
  t.table_name,
  t.return_type,
  t.description as table_description,
  f.field_name,
  f.field_type,
  f.field_type_normalized,
  f.is_nullable,
  f.description as field_description
FROM
    hasura.table_metadata t
    JOIN hasura.struct_type_field f ON (t.return_type = f.struct_type_name)
    ORDER BY table_name, field_name;
