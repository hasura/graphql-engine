select
  coalesce(columns.columns, '[]') as columns,
  coalesce(pk.columns, '[]') as primary_key_columns,
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
          json_build_object(
            'name',
            json_build_object(
              'name',
              ty.typname,
              'schema',
              ty.typnamespace::regnamespace::text
            ),
            'oid',
            ty.oid :: int ,
            'sqlName',
            pg_catalog.format_type(td.atttypid, td.atttypmod),
            'dimension',
            td.attndims
          ),
          'is_nullable',
          is_nullable :: boolean
        )
      ) as columns
    from
      information_schema.columns c
      left outer join (
         select pc.relnamespace,
                pc.relname,
                pa.attname,
                pa.attndims,
                pa.atttypid,
                pa.atttypmod
         from pg_attribute pa
         left join pg_class pc
         on pa.attrelid = pc.oid
      ) td on
      ( c.table_schema::regnamespace::oid = td.relnamespace
        AND c.table_name = td.relname
        AND c.column_name = td.attname
      )
      left outer join pg_type ty
      on
      ( ty.typname =
          case
            when c.domain_name is not null then c.domain_name
            else c.udt_name
          end
        AND ty.typnamespace::regnamespace::text =
          case
            when c.domain_name is not null then c.domain_schema
            else c.udt_schema
          end
      )
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
where
  tables.table_schema = $1 AND
  tables.table_name = $2
