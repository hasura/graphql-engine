select
  coalesce(columns.columns, '[]') as columns,
  coalesce(constraints.constraints, '[]') as constraints,
  coalesce(views.view_info, 'null') as view_info
from
  information_schema.tables as tables
  left outer join (
    select
      c.table_schema,
      c.table_name,
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
    select
      cm.table_schema,
      cm.table_name,
      json_agg(
        json_build_object(
          'type', cm.constraint_type,
          'name', cm.constraint_name,
          'cols', cm.columns
        )
      ) as constraints
    from
      (
        select table_name, table_schema,
               constraint_name, columns,
               'PRIMARY KEY' as constraint_type
          from hdb_catalog.hdb_primary_key
        union all
        select table_name, table_schema,
               constraint_name, columns,
               'UNIQUE' as constraint_type
          from hdb_catalog.hdb_unique_constraint
      ) cm
    group by
      cm.table_schema,
      cm.table_name
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
where
  tables.table_schema = $1 AND
  tables.table_name = $2
