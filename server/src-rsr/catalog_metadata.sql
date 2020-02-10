select
  json_build_object(
    'tables', tables.items :: json,
    'relations', relations.items,
    'permissions', permissions.items,
    'event_triggers', event_triggers.items,
    'remote_schemas', remote_schemas.items,
    'functions', functions.items,
    'allowlist_collections', allowlist.item,
    'computed_fields', computed_field.items,
    'custom_types', coalesce((select custom_types from hdb_catalog.hdb_custom_types), '{}'),
    'actions', actions.items
  )
from
  (
    select
      coalesce(jsonb_agg(
        jsonb_build_object(
          'name', jsonb_build_object(
            'name', ht.table_name,
            'schema', ht.table_schema
          ),
          'is_enum', ht.is_enum,
          'is_system_defined', ht.is_system_defined,
          'configuration', ht.configuration,
          'info', t.info
        )
      ), '[]') as items
    from hdb_catalog.hdb_table ht
    left join hdb_catalog.hdb_table_info_agg t using (table_schema, table_name)
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
  ) as computed_field,
  (
    select
      coalesce(
        json_agg(
          json_build_object(
            'name', ha.action_name,
            'definition', ha.action_defn :: json,
            'comment', ha.comment,
            'permissions', p.items
          )
        ),
        '[]'
      ) as items
    from
      hdb_catalog.hdb_action ha
      left join lateral
      (
        select
          coalesce(
            json_agg(
              json_build_object(
                'action', hap.action_name,
                'role', hap.role_name,
                'comment', hap.comment
              )
            ),
            '[]'
          ) as items
          from
              hdb_catalog.hdb_action_permission hap
          where hap.action_name = ha.action_name
      ) p on 'true'
  ) as actions
