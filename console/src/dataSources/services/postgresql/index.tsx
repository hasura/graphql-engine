import React from 'react';
import { Table, TableColumn, ComputedField } from '../../types';
import { QUERY_TYPES, Operations } from '../../common';
import { PGFunction } from './types';
import { DataSourcesAPI, ColumnsInfoResult } from '../..';

export const isTable = (table: Table) => {
  return (
    table.table_type === 'TABLE' ||
    table.table_type === 'PARTITIONED TABLE' ||
    table.table_type === 'FOREIGN TABLE'
  );
};

export const displayTableName = (table: Table) => {
  // TODO: it shouldn't be here
  const tableName = table.table_name;

  return isTable(table) ? <span>{tableName}</span> : <i>{tableName}</i>;
};

export const getTableSupportedQueries = (table: Table) => {
  let supportedQueryTypes: Operations[];

  if (isTable(table)) {
    supportedQueryTypes = QUERY_TYPES;
  } else {
    // is View
    supportedQueryTypes = [];

    // Add insert/update permission if it is insertable/updatable as returned by pg
    if (table.view_info) {
      if (
        table.view_info.is_insertable_into === 'YES' ||
        table.view_info.is_trigger_insertable_into === 'YES'
      ) {
        supportedQueryTypes.push('insert');
      }

      supportedQueryTypes.push('select'); // to maintain order

      if (table.view_info.is_updatable === 'YES') {
        supportedQueryTypes.push('update');
        supportedQueryTypes.push('delete');
      } else {
        if (table.view_info.is_trigger_updatable === 'YES') {
          supportedQueryTypes.push('update');
        }

        if (table.view_info.is_trigger_deletable === 'YES') {
          supportedQueryTypes.push('delete');
        }
      }
    } else {
      supportedQueryTypes.push('select');
    }
  }

  return supportedQueryTypes;
};

export const getColumnType = (column: TableColumn) => {
  let columnType = column.data_type;

  if (columnType === 'USER-DEFINED') {
    columnType = column.udt_name;
  }

  return columnType;
};

export const isColumnAutoIncrement = (column: TableColumn) => {
  const columnDefault = column.column_default;

  const autoIncrementDefaultRegex = /^nextval\('(.*)_seq'::regclass\)$/;

  return !!(
    columnDefault &&
    columnDefault.match(new RegExp(autoIncrementDefaultRegex, 'gi'))
  );
};

const arrayToPostgresArray = (arr: unknown[]) => {
  return `{${arr.join(',')}}`;
};

export const getFunctionSchema = (pgFunction: PGFunction) => {
  return pgFunction.function_schema;
};

export const getFunctionName = (pgFunction: PGFunction) => {
  return pgFunction.function_name;
};

export const getFunctionDefinition = (pgFunction: PGFunction) => {
  return pgFunction.function_definition;
};

export const getSchemaFunctions = (
  allFunctions: PGFunction[],
  fnSchema: string
) => {
  return allFunctions.filter(fn => getFunctionSchema(fn) === fnSchema);
};

export const findFunction = (
  allFunctions: PGFunction[],
  functionName: string,
  functionSchema: string
) => {
  return allFunctions.find(
    f =>
      getFunctionName(f) === functionName &&
      getFunctionSchema(f) === functionSchema
  );
};

export const getGroupedTableComputedFields = (
  table: Table,
  allFunctions: PGFunction[]
) => {
  const groupedComputedFields: {
    scalar: ComputedField[];
    table: ComputedField[];
  } = { scalar: [], table: [] };

  table.computed_fields.forEach(computedField => {
    const computedFieldFnDef = computedField.definition.function;
    const computedFieldFn = findFunction(
      allFunctions,
      computedFieldFnDef.name,
      computedFieldFnDef.schema
    );

    if (computedFieldFn && computedFieldFn.return_type_type === 'b') {
      groupedComputedFields.scalar.push(computedField);
    } else {
      groupedComputedFields.table.push(computedField);
    }
  });

  return groupedComputedFields;
};

const initQueries = {
  schemaList: {
    type: 'select',
    args: {
      table: {
        name: 'schemata',
        schema: 'information_schema',
      },
      columns: ['schema_name'],
      order_by: [{ column: 'schema_name', type: 'asc', nulls: 'last' }],
      where: {
        schema_name: {
          $nin: [
            'information_schema',
            'pg_catalog',
            'hdb_catalog',
            'hdb_views',
          ],
        },
      },
    },
  },
  loadTrackedFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function',
        schema: 'hdb_catalog',
      },
      columns: ['function_name', 'function_schema', 'is_system_defined'],
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
      },
    },
  },
  loadTrackableFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function_agg',
        schema: 'hdb_catalog',
      },
      columns: [
        'function_name',
        'function_schema',
        'has_variadic',
        'function_type',
        'function_definition',
        'return_type_schema',
        'return_type_name',
        'return_type_type',
        'returns_set',
        {
          name: 'return_table_info',
          columns: ['table_schema', 'table_name'],
        },
      ],
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
        has_variadic: false,
        returns_set: true,
        return_type_type: 'c', // COMPOSITE type
        return_table_info: {},
        $or: [
          {
            function_type: {
              $ilike: '%stable%',
            },
          },
          {
            function_type: {
              $ilike: '%immutable%',
            },
          },
        ],
      },
    },
  },
  loadNonTrackableFunctions: {
    type: 'select',
    args: {
      table: {
        name: 'hdb_function_agg',
        schema: 'hdb_catalog',
      },
      columns: [
        'function_name',
        'function_schema',
        'has_variadic',
        'function_type',
        'function_definition',
        'return_type_schema',
        'return_type_name',
        'return_type_type',
        'returns_set',
        {
          name: 'return_table_info',
          columns: ['table_schema', 'table_name'],
        },
      ],
      order_by: [{ column: 'function_name', type: 'asc', nulls: 'last' }],
      where: {
        function_schema: '', // needs to be set later
        $not: {
          has_variadic: false,
          returns_set: true,
          return_type_type: 'c', // COMPOSITE type
          return_table_info: {},
          $or: [
            {
              function_type: {
                $ilike: '%stable%',
              },
            },
            {
              function_type: {
                $ilike: '%immutable%',
              },
            },
          ],
        },
      },
    },
  },
};

const additionalColumnsInfoQuery = (schemaName: string) => ({
  type: 'select',
  args: {
    table: {
      name: 'columns',
      schema: 'information_schema',
    },
    columns: [
      'column_name',
      'table_name',
      'is_generated',
      'is_identity',
      'identity_generation',
    ],
    where: {
      table_schema: {
        $eq: schemaName,
      },
    },
  },
});

type ColumnsInfoPayload = {
  column_name: string;
  table_name: string;
  is_generated: string;
  is_identity: string;
  identity_generation: 'ALWAYS' | 'BY DEFAULT' | null;
};

const parseColumnsInfoResult = (data: ColumnsInfoPayload[]) => {
  let columnsInfo: ColumnsInfoResult = {};
  data
    .filter(
      (info: ColumnsInfoPayload) =>
        info.is_generated !== 'NEVER' || info.is_identity !== 'NO'
    )
    .forEach(
      ({
        column_name,
        table_name,
        is_generated,
        is_identity,
        identity_generation,
      }) => {
        columnsInfo = {
          ...columnsInfo,
          [table_name]: {
            ...columnsInfo[table_name],
            [column_name]: {
              is_generated: is_generated !== 'NEVER',
              is_identity: is_identity !== 'NO',
              identity_generation,
            },
          },
        };
      }
    );
  return columnsInfo;
};

const columnDataTypes = {
  INTEGER: 'integer',
  SERIAL: 'serial',
  BIGINT: 'bigint',
  BIGSERIAL: 'bigserial',
  UUID: 'uuid',
  JSONDTYPE: 'json',
  JSONB: 'jsonb',
  TIMESTAMP: 'timestamp with time zone',
  TIME: 'time with time zone',
  NUMERIC: 'numeric',
  DATE: 'date',
  TIMETZ: 'timetz',
  BOOLEAN: 'boolean',
  TEXT: 'text',
  ARRAY: 'ARRAY',
};

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

export const getFetchTrackedTableFkQuery = (options: {
  schemas: string[];
  tables: Table[];
}) => {
  const whereQuery = generateWhereClause(options);

  return `select
  COALESCE(
    json_agg(
      row_to_json(info)
    ),
    '[]' :: JSON
  ) AS tables
FROM
  (
    select
      hdb_fkc.*,
      fk_ref_table.table_name IS NOT NULL AS is_ref_table_tracked
    from
      hdb_catalog.hdb_table AS ist
      JOIN hdb_catalog.hdb_foreign_key_constraint AS hdb_fkc ON hdb_fkc.table_schema = ist.table_schema
      and hdb_fkc.table_name = ist.table_name
      LEFT OUTER JOIN hdb_catalog.hdb_table AS fk_ref_table ON fk_ref_table.table_schema = hdb_fkc.ref_table_table_schema
      and fk_ref_table.table_name = hdb_fkc.ref_table
    ${whereQuery}
  ) as info
`;
};

export const getFetchTrackedTableReferencedFkQuery = (options: {
  schemas: string[];
  tables: Table[];
}) => {
  const whereQuery = generateWhereClause(options);

  return `select
  COALESCE(
    json_agg(
      row_to_json(info)
    ),
    '[]' :: JSON
  ) AS tables
FROM
  (
    select DISTINCT ON (hdb_fkc.constraint_oid)
      hdb_fkc.*,
      fk_ref_table.table_name IS NOT NULL AS is_table_tracked,
      hdb_uc.constraint_name IS NOT NULL AS is_unique
    from
      hdb_catalog.hdb_table AS ist
      JOIN hdb_catalog.hdb_foreign_key_constraint AS hdb_fkc ON hdb_fkc.ref_table_table_schema = ist.table_schema
      and hdb_fkc.ref_table = ist.table_name
      LEFT OUTER JOIN hdb_catalog.hdb_table AS fk_ref_table ON fk_ref_table.table_schema = hdb_fkc.table_schema
      and fk_ref_table.table_name = hdb_fkc.table_name
      LEFT OUTER JOIN hdb_catalog.hdb_unique_constraint AS hdb_uc ON hdb_uc.table_schema = hdb_fkc.table_schema
      and hdb_uc.table_name = hdb_fkc.table_name and ARRAY(select json_array_elements_text(hdb_uc.columns) ORDER BY json_array_elements_text) = ARRAY(select json_object_keys(hdb_fkc.column_mapping) ORDER BY json_object_keys)
    ${whereQuery}
  ) as info
`;
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

  // TODO: optimise this.
  return `
SELECT
  COALESCE(Json_agg(Row_to_json(info)), '[]' :: json) AS tables
FROM (
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
    row_to_json(isv) AS view_info

  FROM pg_class as pgc
  INNER JOIN pg_namespace as pgn
    ON pgc.relnamespace = pgn.oid

  /* columns */
  /* This is a simplified version of how information_schema.columns was
  ** implemented in postgres 9.5, but modified to support materialized
  ** views.
  */
  LEFT OUTER JOIN pg_attribute AS pga
    ON pga.attrelid = pgc.oid
  LEFT OUTER JOIN (
    SELECT
      current_database() AS table_catalog,
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
      CASE WHEN nco.nspname IS NOT NULL THEN current_database() END AS collation_catalog,
      nco.nspname AS collation_schema,
      co.collname AS collation_name,
      CASE WHEN t.typtype = 'd' THEN current_database() ELSE null END AS domain_catalog,
      CASE WHEN t.typtype = 'd' THEN nt.nspname ELSE null END AS domain_schema,
      CASE WHEN t.typtype = 'd' THEN t.typname ELSE null END AS domain_name,
      current_database() AS udt_catalog,
      coalesce(nbt.nspname, nt.nspname) AS udt_schema,
      coalesce(bt.typname, t.typname) AS udt_name,
      a.attnum AS dtd_identifier,
      CASE WHEN c.relkind = 'r' OR
                     (c.relkind IN ('v', 'f', 'p') AND
                      pg_column_is_updatable(c.oid, a.attnum, false))
           THEN 'YES' ELSE 'NO' END AS is_updatable
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
      current_database() AS table_catalog,
      nc.nspname         AS table_schema,
      c.relname          AS table_name,
      CASE WHEN pg_has_role(c.relowner, 'USAGE') THEN pg_get_viewdef(c.oid) ELSE null END AS view_definition,
      CASE WHEN 'check_option=cascaded' = ANY (c.reloptions) THEN 'CASCADED'
           WHEN 'check_option=local'    = ANY (c.reloptions) THEN 'LOCAL'
           ELSE 'NONE'
      END AS check_option,
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
    ${whereQuery}
  GROUP BY pgc.oid, pgn.nspname, pgc.relname, table_type, isv.*
) AS info;
`;
};

const commonDataTypes = [
  {
    name: 'Integer',
    value: 'integer',
    description: 'signed four-byte integer',
    hasuraDatatype: 'integer',
  },
  {
    name: 'Integer (auto-increment)',
    value: 'serial',
    description: 'autoincrementing four-byte integer',
    hasuraDatatype: null,
  },
  {
    name: 'Text',
    value: 'text',
    description: 'variable-length character string',
    hasuraDatatype: 'text',
  },
  {
    name: 'Boolean',
    value: 'boolean',
    description: 'logical Boolean (true/false)',
    hasuraDatatype: 'boolean',
  },
  {
    name: 'Numeric',
    value: 'numeric',
    description: 'exact numeric of selected precision',
    hasuraDatatype: 'numeric',
  },
  {
    name: 'Timestamp',
    value: 'timestamptz',
    description: 'date and time, including time zone',
    hasuraDatatype: 'timestamp with time zone',
  },
  {
    name: 'Time',
    value: 'timetz',
    description: 'time of day (no time zone)',
    hasuraDatatype: 'time with time zone',
  },
  {
    name: 'Date',
    value: 'date',
    description: 'calendar date (year, month, day)',
    hasuraDatatype: 'date',
  },
  {
    name: 'UUID',
    value: 'uuid',
    description: 'universal unique identifier',
    hasuraDatatype: 'uuid',
  },
  {
    name: 'JSONB',
    value: 'jsonb',
    description: 'binary format JSON data',
    hasuraDatatype: 'jsonb',
  },
  {
    name: 'Big Integer',
    value: 'bigint',
    description: 'signed eight-byte integer',
    hasuraDatatype: 'bigint',
  },
  {
    name: 'Big Integer (auto-increment)',
    value: 'bigserial',
    description: 'autoincrementing eight-byte integer',
    hasuraDatatype: null,
  },
];

export const fetchColumnTypesQuery = `
SELECT
  string_agg(t.typname, ',') as "Type Name",
  string_agg(pg_catalog.format_type(t.oid, NULL), ',') as "Display Name",
  string_agg(coalesce(pg_catalog.obj_description(t.oid, 'pg_type'), ''), ':') as "Descriptions",
  t.typcategory
FROM pg_catalog.pg_type t
     LEFT JOIN pg_catalog.pg_namespace n ON n.oid = t.typnamespace
WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid))
  AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = t.typelem AND el.typarray = t.oid)
  AND pg_catalog.pg_type_is_visible(t.oid)
  AND t.typname != 'unknown'
  AND t.typcategory != 'P'
GROUP BY t.typcategory;`;

export const fetchColumnDefaultFunctions = (schema = 'public') => `
SELECT string_agg(pgp.proname, ','),
  t.typname as "Type"
from pg_proc pgp
JOIN pg_type t
ON pgp.prorettype = t.oid
JOIN pg_namespace pgn
ON pgn.oid = pgp.pronamespace
WHERE (t.typrelid = 0 OR (SELECT c.relkind = 'c' FROM pg_catalog.pg_class c WHERE c.oid = t.typrelid))
  AND NOT EXISTS(SELECT 1 FROM pg_catalog.pg_type el WHERE el.oid = t.typelem AND el.typarray = t.oid)
  AND pg_catalog.pg_type_is_visible(t.oid)
  AND t.typname != 'unknown'
  AND t.typcategory != 'P'
  AND (array_length(pgp.proargtypes, 1) = 0)
  AND ( pgn.nspname = '${schema}' OR pgn.nspname = 'pg_catalog' )
  AND pgp.proretset=false
GROUP BY t.typname
ORDER BY t.typname ASC;
`;

export const isSQLFunction = (str: string) => new RegExp(/.*\(\)$/gm).test(str);

export const getEstimateCountQuery = (
  schemaName: string,
  tableName: string
) => {
  return `
SELECT
  reltuples::BIGINT
FROM
  pg_class
WHERE
  oid = (quote_ident('${schemaName}') || '.' || quote_ident('${tableName}'))::regclass::oid
  AND relname = '${tableName}';
`;
};

const isColTypeString = (colType: string) =>
  ['text', 'varchar', 'char', 'bpchar', 'name'].includes(colType);

const cascadeSqlQuery = (sql: string) => {
  if (sql[sql.length - 1] === ';') {
    return `${sql.substr(0, sql.length - 1)} CASCADE;`;
  }
  // SQL might have  a " at the end
  else if (sql[sql.length - 2] === ';') {
    return `${sql.substr(0, sql.length - 2)} CASCADE;`;
  }
  return `${sql} CASCADE;`;
};

const dependecyErrorCode = '2BP01'; // pg dependent error > https://www.postgresql.org/docs/current/errcodes-appendix.html

export const postgres: DataSourcesAPI = {
  isTable,
  displayTableName,
  getFunctionSchema,
  getFunctionName,
  getFunctionDefinition,
  getSchemaFunctions,
  findFunction,
  getGroupedTableComputedFields,
  isColumnAutoIncrement,
  getTableSupportedQueries,
  getColumnType,
  arrayToPostgresArray,
  initQueries,
  additionalColumnsInfoQuery,
  parseColumnsInfoResult,
  columnDataTypes,
  generateWhereClause,
  getFetchTrackedTableFkQuery,
  getFetchTrackedTableReferencedFkQuery,
  getFetchTablesListQuery,
  commonDataTypes,
  fetchColumnTypesQuery,
  fetchColumnDefaultFunctions,
  isSQLFunction,
  getEstimateCountQuery,
  isColTypeString,
  cascadeSqlQuery,
  dependecyErrorCode,
};
