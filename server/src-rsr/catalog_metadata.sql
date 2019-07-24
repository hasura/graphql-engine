select
  json_build_object(
    'tables', tables.items,
    'relations', relations.items,
    'permissions', permissions.items,
    'event_triggers', event_triggers.items,
    'remote_schemas', remote_schemas.items,
    'functions', functions.items,
    'foreign_keys', foreign_keys.items,
    'allowlist_collections', allowlist.items,
    'remote_relationships', remote_relationships.items
  )
from
  (
    select
      coalesce(json_agg(
        json_build_object(
          'table',
          json_build_object(
            'name', ht.table_name,
            'schema', ht.table_schema
          ),
          'system_defined', ht.is_system_defined,
          'info', tables.info
        )
      ), '[]') as items
    from
      hdb_catalog.hdb_table as ht
      left outer join (
        select
          table_schema,
          table_name,
          json_build_object(
            'name',
            json_build_object(
              'schema', table_schema,
              'name', table_name
            ),
            'columns', columns,
            'primary_key_columns', primary_key_columns,
            'constraints', constraints,
            'view_info', view_info
          ) as info
        from
          hdb_catalog.hdb_table_info_agg
      ) as tables on (
        tables.table_schema = ht.table_schema
        and tables.table_name = ht.table_name
      )
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
            'info', function_info
          ) as info
        from
          hdb_catalog.hdb_function hf
        left outer join
            hdb_catalog.hdb_function_info_agg hf_agg on
            ( hf_agg.function_name = hf.function_name
              and hf_agg.function_schema = hf.function_schema
            )
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
      coalesce(json_agg(hqc.collection_defn), '[]') as items
    from hdb_catalog.hdb_allowlist ha
    left outer join
         hdb_catalog.hdb_query_collection hqc
         on (hqc.collection_name = ha.collection_name)
  ) as allowlist,
  (
    select coalesce(json_agg(configuration),'[]') as items
    from hdb_catalog.hdb_remote_relationship
  ) as remote_relationships
