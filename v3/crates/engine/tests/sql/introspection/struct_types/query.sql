SELECT
    s.name,
    s.description,
    f.fields
FROM
    hasura.struct_type s
LEFT OUTER JOIN
    (SELECT struct_type_name,
       ARRAY_AGG(
         STRUCT(
          field_name as field_name,
          field_type as field_type,
          field_type_normalized as field_type_normalized,
          is_nullable as is_nullable,
          description as description
        )
        ORDER BY field_name
      ) as fields
       FROM
        hasura.struct_type_field
        GROUP BY struct_type_name
    ) f
ON
    s.name = f.struct_type_name
ORDER BY
    struct_type_name;
