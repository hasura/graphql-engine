SELECT
    f.function_name,
    f.return_type,
    f.description,
    a.arguments
FROM
    hasura.table_valued_function f
LEFT OUTER JOIN
    (SELECT function_name,
       ARRAY_AGG(
         STRUCT(
          argument_name as name,
          argument_position as position,
          argument_type as argument_type,
          argument_type_normalized as argument_type_normalized,
          is_nullable as is_nullable,
          description as description
        )
      ) as arguments
       FROM
        hasura.table_valued_function_argument
        GROUP BY function_name
    ) a
ON
    f.function_name = a.function_name
ORDER BY
    function_name;
