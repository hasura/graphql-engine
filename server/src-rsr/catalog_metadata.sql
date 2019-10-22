select
  json_build_object(
    'tables', tables.items,
    'relations', relations.items,
    'permissions', permissions.items,
    'event_triggers', event_triggers.items,
    'remote_schemas', remote_schemas.items,
    'functions', functions.items,
    'foreign_keys', foreign_keys.items,
    'allowlist_collections', allowlist.item,
    'computed_fields', computed_field.items
  )
from
  (
    select
      coalesce(json_agg(
        json_build_object(
          'name', json_build_object(
            'name', ht.table_name,
            'schema', ht.table_schema
          ),
          'is_enum', ht.is_enum,
          'is_system_defined', ht.is_system_defined,
          'configuration', ht.configuration,
          'info', t.info
        )
      ), '[]') as items
    from hdb_catalog.hdb_table as ht
    left outer join (
      select
        table_schema,
        table_name,
        jsonb_build_object(
          'description', description,
          'columns', columns,
          'primary_key_columns', primary_key_columns,
          'constraints', constraints,
          'view_info', view_info
        ) as info
      from hdb_catalog.hdb_table_info_agg
    ) as t using (table_schema, table_name)
  ) as tables,
  (
    select
      coalesce(
        json_agg(
          json_build_object(
            'table',
            json_build_object(
              'schema', table_schema,
              'name', table_name
            ),
            'rel_name', rel_name,
            'rel_type', rel_type,
            'def', rel_def :: json,
            'comment', comment
          )
        ),
        '[]'
      ) as items
    from
      hdb_catalog.hdb_relationship
  ) as relations,
  (
    select
      coalesce(
        json_agg(
          json_build_object(
            'table',
            json_build_object(
              'schema', table_schema,
              'name', table_name
            ),
            'role', role_name,
            'perm_type', perm_type,
            'def', perm_def :: json,
            'comment', comment
          )
        ),
        '[]'
      ) as items
    from
      hdb_catalog.hdb_permission
  ) as permissions,
  (
    select
      coalesce(
        json_agg(
          json_build_object(
            'table',
            json_build_object(
              'schema', schema_name,
              'name', table_name
            ),
            'name', name,
            'def', configuration :: json
          )
        ),
        '[]'
      ) as items
    from
      hdb_catalog.event_triggers
  ) as event_triggers,
  (
    select
      coalesce(
        json_agg(
          json_build_object(
            'name',
            name,
            'definition', definition :: json,
            'comment', comment
          )
        ),
        '[]'
      ) as items
    from
      hdb_catalog.remote_schemas
  ) as remote_schemas,
  (
    select
      coalesce(json_agg(q.info), '[]') as items
    from
        (
        select
          json_build_object(
            'function',
            json_build_object(
              'schema', hf.function_schema,
              'name', hf.function_name
            ),
            'configuration', hf.configuration,
            'is_system_defined', hf.is_system_defined,
            'info', hf_agg.function_info
          ) as info
        from
          hdb_catalog.hdb_function hf
        left join lateral
            (
              select coalesce(json_agg(function_info), '[]') as function_info
              from hdb_catalog.hdb_function_info_agg
               where function_name = hf.function_name
                     and function_schema = hf.function_schema
            ) hf_agg on 'true'
      ) as q
   ) as functions,
  (
    select
      coalesce(json_agg(foreign_key.info), '[]') as items
    from
      (
        select
          json_build_object(
            'table',
            json_build_object(
              'schema', f.table_schema,
              'name', f.table_name
            ),
            'ref_table',
            json_build_object(
              'schema', f.ref_table_table_schema,
              'name', f.ref_table
            ),
            'oid', f.constraint_oid,
            'constraint', f.constraint_name,
            'column_mapping', f.column_mapping
          ) as info
        from
         hdb_catalog.hdb_foreign_key_constraint f
         left outer join hdb_catalog.hdb_table ht
         on ( ht.table_schema = f.table_schema
              and ht.table_name = f.table_name
            )
      ) as foreign_key
  ) as foreign_keys,
  (
    select
      coalesce(json_agg(hqc.collection_defn), '[]') as item
    from hdb_catalog.hdb_allowlist ha
    left outer join
         hdb_catalog.hdb_query_collection hqc
         on (hqc.collection_name = ha.collection_name)
  ) as allowlist,
  (
    select
      coalesce(json_agg(
        json_build_object('computed_field', cc.computed_field,
                          'function_info', fi.function_info
                         )
      ), '[]') as items
    from
      (
        select json_build_object(
          'table', jsonb_build_object('name', hcc.table_name,'schema', hcc.table_schema),
          'name', hcc.computed_field_name,
          'definition', hcc.definition,
          'comment', hcc.comment
        ) as computed_field,
        hccf.function_name,
        hccf.function_schema
        from hdb_catalog.hdb_computed_field hcc
        left outer join
             hdb_catalog.hdb_computed_field_function hccf
             on ( hcc.table_name = hccf.table_name
                 and hcc.table_schema = hccf.table_schema
                 and hcc.computed_field_name = hccf.computed_field_name
                )
      ) cc
    left join lateral
      (
        select coalesce(json_agg(function_info), '[]') as function_info
        from hdb_catalog.hdb_function_info_agg
        where function_name = cc.function_name and function_schema = cc.function_schema
      ) fi on 'true'
  ) as computed_field
