CREATE OR REPLACE VIEW hdb_catalog.hdb_table_info_agg AS (
select
  tables.table_name as table_name,
  tables.table_schema as table_schema,
  coalesce(columns.columns, '[]') as columns,
  coalesce(pk.columns, '[]') as primary_key_columns,
  coalesce(constraints.constraints, '[]') as constraints,
  coalesce(views.view_info, 'null') as view_info
from
  information_schema.tables as tables
  left outer join (
    select
      c.table_name,
      c.table_schema,
      json_agg(
        json_build_object(
          'name',
          column_name,
          'type',
          udt_name,
          'is_nullable',
          is_nullable :: boolean
        )
      ) as columns
    from
      information_schema.columns c
    group by
      c.table_schema,
      c.table_name
  ) columns on (
    tables.table_schema = columns.table_schema
    AND tables.table_name = columns.table_name
  )
  left outer join (
    select * from hdb_catalog.hdb_primary_key
  ) pk on (
    tables.table_schema = pk.table_schema
    AND tables.table_name = pk.table_name
  )
  left outer join (
    select
      c.table_schema,
      c.table_name,
      json_agg(constraint_name) as constraints
    from
      information_schema.table_constraints c
    where
      c.constraint_type = 'UNIQUE'
      or c.constraint_type = 'PRIMARY KEY'
    group by
      c.table_schema,
      c.table_name
  ) constraints on (
    tables.table_schema = constraints.table_schema
    AND tables.table_name = constraints.table_name
  )
  left outer join (
    select
      table_schema,
      table_name,
      json_build_object(
        'is_updatable',
        (is_updatable::boolean OR is_trigger_updatable::boolean),
        'is_deletable',
        (is_updatable::boolean OR is_trigger_deletable::boolean),
        'is_insertable',
        (is_insertable_into::boolean OR is_trigger_insertable_into::boolean)
      ) as view_info
    from
      information_schema.views v
  ) views on (
    tables.table_schema = views.table_schema
    AND tables.table_name = views.table_name
  )
);

CREATE OR REPLACE VIEW hdb_catalog.hdb_function_info_agg AS (
  SELECT
    function_name,
    function_schema,
    row_to_json (
      (
        SELECT
          e
          FROM
              (
                SELECT
                  has_variadic,
                  function_type,
                  return_type_schema,
                  return_type_name,
                  return_type_type,
                  returns_set,
                  input_arg_types,
                  input_arg_names,
                  exists(
                    SELECT
                      1
                      FROM
                          information_schema.tables
                     WHERE
                table_schema = return_type_schema
            AND table_name = return_type_name
                  ) AS returns_table
              ) AS e
      )
    ) AS "function_info"
    FROM
        hdb_catalog.hdb_function_agg
);
