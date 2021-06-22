import { Table } from '../../types';

const generateWhereClause = (
  options: { schemas: string[]; tables: Table[] },
  sqlTableName = 'ist.table_name',
  sqlSchemaName = 'ist.table_schema',
  clausePrefix = 'where'
) => {
  let whereClause = '';

  const whereCondtions: string[] = [];
  if (options.schemas) {
    options.schemas.forEach(schemaName => {
      whereCondtions.push(`(${sqlSchemaName}='${schemaName}')`);
    });
  }
  if (options.tables) {
    options.tables.forEach(tableInfo => {
      whereCondtions.push(
        `(${sqlSchemaName}='${tableInfo.table_schema}' and ${sqlTableName}='${tableInfo.table_name}')`
      );
    });
  }

  if (whereCondtions.length > 0) {
    whereClause = clausePrefix;
  }

  whereCondtions.forEach((whereInfo, index) => {
    whereClause += ` ${whereInfo}`;
    if (index + 1 !== whereCondtions.length) {
      whereClause += ' or';
    }
  });

  return whereClause;
};

export const getFetchTablesListQuery = (options: {
  schemas: string[];
  tables: Table[];
}) => {
  const whereQuery = generateWhereClause(
    options,
    'pgc.relname',
    'pgn.nspname',
    'and'
  );
  return `
  SELECT
    COALESCE(Json_agg(Row_to_json(info)), '[]' :: json) AS tables
  FROM (
    with shards as (select array(select pgc.relname || '_' || pgs.shardid from pg_dist_shard as pgs inner join pg_class as pgc on pgc.oid = pgs.logicalrelid) as names),
    partitions as (
      select array(
        WITH partitioned_tables AS (SELECT array(SELECT oid FROM pg_class WHERE relkind = 'p') AS parent_tables)
        SELECT
        child.relname       AS partition
    FROM partitioned_tables, pg_inherits
        JOIN pg_class child             ON pg_inherits.inhrelid   = child.oid
        JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
    ${generateWhereClause(
      options,
      'child.relname',
      'nmsp_child.nspname',
      'where'
    )}
    AND pg_inherits.inhparent = ANY (partitioned_tables.parent_tables)
      ) as names
      )
    SELECT
      pgn.nspname as table_schema,
      pgc.relname as table_name,
      case
        when pgc.relkind = 'r' then 'TABLE'
        when pgc.relkind = 'f' then 'FOREIGN TABLE'
        when pgc.relkind = 'v' then 'VIEW'
        when pgc.relkind = 'm' then 'MATERIALIZED VIEW'
        when pgc.relkind = 'p' then 'PARTITIONED TABLE'
      end as table_type,
      obj_description(pgc.oid) AS comment,
      COALESCE(json_agg(DISTINCT row_to_json(isc) :: jsonb || jsonb_build_object('comment', col_description(pga.attrelid, pga.attnum))) filter (WHERE isc.column_name IS NOT NULL), '[]' :: json) AS columns,
      COALESCE(json_agg(DISTINCT row_to_json(ist) :: jsonb || jsonb_build_object('comment', obj_description(pgt.oid))) filter (WHERE ist.trigger_name IS NOT NULL), '[]' :: json) AS triggers,
      row_to_json(isv) AS view_info,
      case
        when cts.citus_table_type = 'reference' then 'reference'
        when cts.citus_table_type = 'distributed' then 'distributed' 
        else 'local'
      end as citus_table_type
      FROM partitions, shards, pg_class as pgc
      INNER JOIN pg_namespace as pgn
        ON pgc.relnamespace = pgn.oid
    /* columns */
    /* This is a simplified version of how information_schema.columns was
    ** implemented in postgres 9.5, but modified to support materialized
    ** views.
    */
    LEFT JOIN citus_tables as cts
      ON pgc.oid = cts.table_name
    LEFT OUTER JOIN pg_attribute AS pga
      ON pga.attrelid = pgc.oid
    LEFT OUTER JOIN (
      SELECT
        nc.nspname         AS table_schema,
        c.relname          AS table_name,
        a.attname          AS column_name,
        a.attnum           AS ordinal_position,
        pg_get_expr(ad.adbin, ad.adrelid) AS column_default,
        CASE WHEN a.attnotnull OR (t.typtype = 'd' AND t.typnotnull) THEN 'NO' ELSE 'YES' END AS is_nullable,
        CASE WHEN t.typtype = 'd' THEN
          CASE WHEN bt.typelem <> 0 AND bt.typlen = -1 THEN 'ARRAY'
               WHEN nbt.nspname = 'pg_catalog' THEN format_type(t.typbasetype, null)
               ELSE 'USER-DEFINED' END
        ELSE
          CASE WHEN t.typelem <> 0 AND t.typlen = -1 THEN 'ARRAY'
               WHEN nt.nspname = 'pg_catalog' THEN format_type(a.atttypid, null)
               ELSE 'USER-DEFINED' END
        END AS data_type,
        coalesce(bt.typname, t.typname) AS data_type_name
      FROM (pg_attribute a LEFT JOIN pg_attrdef ad ON attrelid = adrelid AND attnum = adnum)
        JOIN (pg_class c JOIN pg_namespace nc ON (c.relnamespace = nc.oid)) ON a.attrelid = c.oid
        JOIN (pg_type t JOIN pg_namespace nt ON (t.typnamespace = nt.oid)) ON a.atttypid = t.oid
        LEFT JOIN (pg_type bt JOIN pg_namespace nbt ON (bt.typnamespace = nbt.oid))
          ON (t.typtype = 'd' AND t.typbasetype = bt.oid)
        LEFT JOIN (pg_collation co JOIN pg_namespace nco ON (co.collnamespace = nco.oid))
          ON a.attcollation = co.oid AND (nco.nspname, co.collname) <> ('pg_catalog', 'default')
      WHERE (NOT pg_is_other_temp_schema(nc.oid))
        AND a.attnum > 0 AND NOT a.attisdropped AND c.relkind in ('r', 'v', 'm', 'f', 'p')
        AND (pg_has_role(c.relowner, 'USAGE')
             OR has_column_privilege(c.oid, a.attnum,
                                     'SELECT, INSERT, UPDATE, REFERENCES'))
    ) AS isc
      ON  isc.table_schema = pgn.nspname
      AND isc.table_name   = pgc.relname
      AND isc.column_name  = pga.attname
  
    /* triggers */
    LEFT OUTER JOIN pg_trigger AS pgt
      ON pgt.tgrelid = pgc.oid
    LEFT OUTER JOIN information_schema.triggers AS ist
      ON  ist.event_object_schema = pgn.nspname
      AND ist.event_object_table  = pgc.relname
      AND ist.trigger_name        = pgt.tgname
  
    /* This is a simplified version of how information_schema.views was
    ** implemented in postgres 9.5, but modified to support materialized
    ** views.
    */
    LEFT OUTER JOIN (
      SELECT
        nc.nspname         AS table_schema,
        c.relname          AS table_name,
        CASE WHEN pg_has_role(c.relowner, 'USAGE') THEN pg_get_viewdef(c.oid) ELSE null END AS view_definition,
        CASE WHEN pg_relation_is_updatable(c.oid, false) & 20 = 20 THEN 'YES' ELSE 'NO' END AS is_updatable,
        CASE WHEN pg_relation_is_updatable(c.oid, false) &  8 =  8 THEN 'YES' ELSE 'NO' END AS is_insertable_into,
        CASE WHEN EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid = c.oid AND tgtype & 81 = 81) THEN 'YES' ELSE 'NO' END AS is_trigger_updatable,
        CASE WHEN EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid = c.oid AND tgtype & 73 = 73) THEN 'YES' ELSE 'NO' END AS is_trigger_deletable,
        CASE WHEN EXISTS (SELECT 1 FROM pg_trigger WHERE tgrelid = c.oid AND tgtype & 69 = 69) THEN 'YES' ELSE 'NO' END AS is_trigger_insertable_into
      FROM pg_namespace nc, pg_class c
      WHERE c.relnamespace = nc.oid
        AND c.relkind in ('v', 'm')
        AND (NOT pg_is_other_temp_schema(nc.oid))
        AND (pg_has_role(c.relowner, 'USAGE')
             OR has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
             OR has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES'))
    ) AS isv
      ON  isv.table_schema = pgn.nspname
      AND isv.table_name   = pgc.relname
  
    WHERE
      pgc.relkind IN ('r', 'v', 'f', 'm', 'p')
      and NOT (pgc.relname = ANY (partitions.names)) 
      and NOT (pgc.relname = ANY (shards.names))
      and pgc.relname != 'citus_tables'
      ${whereQuery}      
    GROUP BY pgc.oid, pgn.nspname, pgc.relname, table_type, isv.*, cts.citus_table_type
  ) AS info;
  `;
};

export const schemaListSql = (
  schemas?: string[]
) => `SELECT schema_name FROM information_schema.schemata WHERE
schema_name NOT IN ('information_schema', 'hdb_catalog', 'hdb_views', 'columnar', 'citus', 'citus_internal') AND schema_name NOT LIKE 'pg_%'
${schemas?.length ? ` AND schema_name IN (${schemas.join(',')})` : ''}
ORDER BY schema_name ASC;`;

const trackableFunctionsWhere = `
AND has_variadic = FALSE
AND returns_set = TRUE
AND return_type_type = 'c'
`;

const nonTrackableFunctionsWhere = `
AND NOT (
  has_variadic = false
  AND returns_set = TRUE
  AND return_type_type = 'c'
)
`;

const functionWhereStatement = {
  trackable: trackableFunctionsWhere,
  'non-trackable': nonTrackableFunctionsWhere,
};

export const getFunctionDefinitionSql = (
  schemaName: string,
  functionName?: string | null,
  type?: keyof typeof functionWhereStatement
) => `
SELECT
COALESCE(
  json_agg(
    Row_to_json(functions)
  ),
  '[]' :: JSON
) from (
SELECT * FROM (
SELECT p.proname::text AS function_name,
pn.nspname::text AS function_schema,
pd.description,
    CASE
        WHEN p.provariadic = 0::oid THEN false
        ELSE true
    END AS has_variadic,
    CASE
        WHEN p.provolatile::text = 'i'::character(1)::text THEN 'IMMUTABLE'::text
        WHEN p.provolatile::text = 's'::character(1)::text THEN 'STABLE'::text
        WHEN p.provolatile::text = 'v'::character(1)::text THEN 'VOLATILE'::text
        ELSE NULL::text
    END AS function_type,
pg_get_functiondef(p.oid) AS function_definition,
rtn.nspname::text AS return_type_schema,
rt.typname::text AS return_type_name,
rt.typtype::text AS return_type_type,
p.proretset AS returns_set,
( SELECT COALESCE(json_agg(json_build_object('schema', q.schema, 'name', q.name, 'type', q.type)), '[]'::json) AS "coalesce"
       FROM ( SELECT pt.typname AS name,
                pns.nspname AS schema,
                pt.typtype AS type,
                pat.ordinality
               FROM unnest(COALESCE(p.proallargtypes, p.proargtypes::oid[])) WITH ORDINALITY pat(oid, ordinality)
                 LEFT JOIN pg_type pt ON pt.oid = pat.oid
                 LEFT JOIN pg_namespace pns ON pt.typnamespace = pns.oid
              ORDER BY pat.ordinality) q) AS input_arg_types,
to_json(COALESCE(p.proargnames, ARRAY[]::text[])) AS input_arg_names,
p.pronargdefaults AS default_args,
p.oid::integer AS function_oid
FROM pg_proc p
 JOIN pg_namespace pn ON pn.oid = p.pronamespace
JOIN pg_type rt ON rt.oid = p.prorettype
JOIN pg_namespace rtn ON rtn.oid = rt.typnamespace
JOIN pg_language plang ON p.prolang = plang.oid
LEFT JOIN pg_description pd ON p.oid = pd.objoid
WHERE
plang.lanname = 'plpgsql' AND 
pn.nspname::text !~~ 'pg_%'::text
AND(pn.nspname::text <> ALL (ARRAY ['information_schema'::text]))
AND NOT(EXISTS (
  SELECT
    1 FROM pg_aggregate
  WHERE
    pg_aggregate.aggfnoid::oid = p.oid))) as info
WHERE function_schema='${schemaName}'
${functionName ? `AND function_name='${functionName}'` : ''}
${type ? functionWhereStatement[type] : ''}
ORDER BY function_name ASC
${functionName ? 'LIMIT 1' : ''}
) as functions;
`;
