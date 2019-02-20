select row_to_json(types) as types
from (
  select
    t.oid :: integer as oid,  
    json_build_object(
      'name',
      t.typname,
      'schema',
      ns.nspname
    ) as name,
    pg_catalog.format_type(t.oid,NULL) as sql_name,
    case 
      when arr_elem.oid is not null then 
        json_build_object(
          'type',
          'array',
          'elem_oid',
          arr_elem.oid :: integer
        )
      when t.typtype = 'b' then
        json_build_object(
          'type',
          'base'
        )
      when t.typtype = 'e' then 
        json_build_object(
          'type',
          'enum',
          'possible_values',
          ( select array_agg(enumlabel order by enumsortorder) from pg_enum where enumtypid=t.oid )
        )

      when t.typtype = 'c' then
        json_build_object(
          'type',
           'composite',
          'fields',
           ( select json_agg( ( select json_build_object( attname, ( select row_to_json(x) from (select atttypid :: integer as oid, attndims as dimension ) x ) ) ) ) from pg_attribute where attrelid = t.typrelid )
        )

      when t.typtype = 'd' then 
        json_build_object(
          'type',
          'domain',
          'base_type',
          json_build_object(
            'oid',
            t.typbasetype:: integer,
            'dimension',
            t.typndims
          ) 
        )   
      when t.typtype = 'p' then
        json_build_object(
          'type',
          'pseudo'
        )
      when t.typtype = 'r' then
        json_build_object(
          'type',
          'range'
        )
      else null
    end as detail
  from pg_type t 
     left outer join pg_namespace ns
     on t.typnamespace = ns.oid
     left outer join pg_type arr_elem
     on t.oid = arr_elem.typarray
  ) types
