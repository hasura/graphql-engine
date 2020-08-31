import { Table } from '../../types';
import { isColTypeString } from '.';
import { FrequentlyUsedColumn } from '../../../components/Services/Data/Common/Components/FrequentlyUsedColumnSelector';

const sqlEscapeText = (rawText: string) => {
  let text = rawText;

  if (text) {
    text = text.replace(/'/g, "\\'");
  }

  return `E'${text}'`;
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
        coalesce(bt.typname, t.typname) AS udt_name
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
      ${whereQuery}
    GROUP BY pgc.oid, pgn.nspname, pgc.relname, table_type, isv.*
  ) AS info;
  `;
};

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

export const isSQLFunction = (str: string | undefined) =>
  new RegExp(/.*\(\)$/gm).test(str || '');

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

export const cascadeSqlQuery = (sql: string) => {
  if (sql[sql.length - 1] === ';') {
    return `${sql.substr(0, sql.length - 1)} CASCADE;`;
  }
  // SQL might have  a " at the end
  else if (sql[sql.length - 2] === ';') {
    return `${sql.substr(0, sql.length - 2)} CASCADE;`;
  }
  return `${sql} CASCADE;`;
};

type Col = {
  name: string;
  type: string;
  nullable: boolean;
  default?: { value: string };
  dependentSQLGenerator?: (...args: any[]) => string;
};

export const getCreateTableQueries = (
  currentSchema: string,
  tableName: string,
  columns: Col[],
  primaryKeys: (number | string)[],
  foreignKeys: any[],
  uniqueKeys: any[],
  checkConstraints: any[],
  tableComment?: string
) => {
  const currentCols = columns.filter(c => c.name !== '');
  let hasUUIDDefault = false;

  const pKeys = primaryKeys
    .filter(p => p !== '')
    .map(p => currentCols[p as number].name);

  const columnSpecificSql: any[] = [];

  let tableDefSql = '';
  for (let i = 0; i < currentCols.length; i++) {
    tableDefSql += `"${currentCols[i].name}" ${currentCols[i].type}`;

    // check if column is nullable
    if (!currentCols[i].nullable) {
      tableDefSql += ' NOT NULL';
    }

    // check if column has a default value
    if (
      currentCols[i].default !== undefined &&
      currentCols[i].default?.value !== ''
    ) {
      if (
        isColTypeString(currentCols[i].type) &&
        !isSQLFunction(currentCols[i]?.default?.value)
      ) {
        // if a column type is text and if it has a non-func default value, add a single quote by default
        tableDefSql += ` DEFAULT '${currentCols[i]?.default?.value}'`;
      } else {
        tableDefSql += ` DEFAULT ${currentCols[i]?.default?.value}`;
      }

      if (currentCols[i].type === 'uuid') {
        hasUUIDDefault = true;
      }
    }

    // check if column has dependent sql
    const depGen = currentCols[i].dependentSQLGenerator;
    if (depGen) {
      const dependentSql = depGen(
        currentSchema,
        tableName,
        currentCols[i].name
      );
      columnSpecificSql.push(dependentSql);
    }

    tableDefSql += i === currentCols.length - 1 ? '' : ', ';
  }

  // add primary key
  if (pKeys.length > 0) {
    tableDefSql += ', PRIMARY KEY (';
    tableDefSql += pKeys.map(col => `"${col}"`).join(',');
    tableDefSql += ') ';
  }

  // add foreign keys
  const numFks = foreignKeys.length;
  let fkDupColumn = null;
  if (numFks > 1) {
    foreignKeys.forEach((fk, _i) => {
      if (_i === numFks - 1) {
        return;
      }

      const { colMappings, refTableName, onUpdate, onDelete } = fk;

      const mappingObj: Record<string, string> = {};
      const rCols: string[] = [];
      const lCols: string[] = [];

      colMappings
        .slice(0, -1)
        .forEach((cm: { column: string | number; refColumn: string }) => {
          if (mappingObj[cm.column] !== undefined) {
            fkDupColumn = columns[cm.column as number].name;
          }

          mappingObj[cm.column] = cm.refColumn;
          lCols.push(`"${columns[cm.column as number].name}"`);
          rCols.push(`"${cm.refColumn}"`);
        });

      if (lCols.length === 0) {
        return;
      }

      tableDefSql += `, FOREIGN KEY (${lCols.join(', ')}) REFERENCES "${
        fk.refSchemaName
      }"."${refTableName}"(${rCols.join(
        ', '
      )}) ON UPDATE ${onUpdate} ON DELETE ${onDelete}`;
    });
  }

  if (fkDupColumn) {
    return {
      error: `The column "${fkDupColumn}" seems to be referencing multiple foreign columns`,
    };
  }

  // add unique keys
  const numUniqueConstraints = uniqueKeys.length;
  if (numUniqueConstraints > 0) {
    uniqueKeys.forEach(uk => {
      if (!uk.length) {
        return;
      }

      const uniqueColumns = uk.map((c: number) => `"${columns[c].name}"`);
      tableDefSql += `, UNIQUE (${uniqueColumns.join(', ')})`;
    });
  }

  // add check constraints
  if (checkConstraints.length > 0) {
    checkConstraints.forEach(constraint => {
      if (!constraint.name || !constraint.check) {
        return;
      }

      tableDefSql += `, CONSTRAINT "${constraint.name}" CHECK (${constraint.check})`;
    });
  }

  let sqlCreateTable = `CREATE TABLE "${currentSchema}"."${tableName}" (${tableDefSql});`;

  // add comment
  if (tableComment && tableComment !== '') {
    sqlCreateTable += `COMMENT ON TABLE "${currentSchema}".${tableName} IS ${sqlEscapeText(
      tableComment
    )};`;
  }

  if (columnSpecificSql.length) {
    columnSpecificSql.forEach(csql => {
      sqlCreateTable += csql.upSql;
    });
  }

  const sqlQueries: string[] = [sqlCreateTable];

  if (hasUUIDDefault) {
    const sqlCreateExtension = 'CREATE EXTENSION IF NOT EXISTS pgcrypto;';

    sqlQueries.push(sqlCreateExtension);
  }

  return sqlQueries;
};

export const getDropTableSql = (schema: string, table: string) => {
  return `DROP TABLE "${schema}"."${table}"`;
};

export const getStatementTimeoutSql = (statementTimeoutInSecs: number) => {
  return `SET LOCAL statement_timeout = ${statementTimeoutInSecs * 1000};`;
};

export const getDropSchemaSql = (schemaName: string) =>
  `drop schema "${schemaName}" cascade;`;

export const getCreateSchemaSql = (schemaName: string) =>
  `create schema "${schemaName}";`;

export const getAlterForeignKeySql = (
  from: {
    tableName: string;
    schemaName: string;
    columns: string[];
  },
  to: {
    tableName: string;
    schemaName: string;
    columns: string[];
  },
  dropConstraint: string,
  newConstraint: string,
  onUpdate: string,
  onDelete: string
) => `
  alter table "${from.schemaName}"."${
  from.tableName
}" drop constraint "${dropConstraint}",
  add constraint "${newConstraint}"
  foreign key (${from.columns.join(', ')})
  references "${to.schemaName}"."${to.tableName}"
  (${to.columns.join(', ')}) on update ${onUpdate} on delete ${onDelete};
`;

export const getCreateFKeySql = (
  from: {
    tableName: string;
    schemaName: string;
    columns: string[];
  },
  to: {
    tableName: string;
    schemaName: string;
    columns: string[];
  },
  newConstraint: string,
  onUpdate: string,
  onDelete: string
) => `
  alter table "${from.schemaName}"."${from.tableName}"
  add constraint "${newConstraint}"
  foreign key (${from.columns.join(', ')})
  references "${to.schemaName}"."${to.tableName}"
  (${to.columns.join(', ')}) on update ${onUpdate} on delete ${onDelete};
`;

export const getDropConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string
) => `
  alter table "${schemaName}"."${tableName}" drop constraint "${constraintName}";
`;

export const getRenameTableSql = (
  property = 'table',
  schemaName: string,
  oldName: string,
  newName: string
) => `
 alter ${property} "${schemaName}"."${oldName}" rename to "${newName}";
`;

export const getDropTriggerSql = (
  tableName: string,
  tableSchema: string,
  triggerName: string
) => `
  DROP TRIGGER "${triggerName}" ON "${tableSchema}"."${tableName}";
`;

export const getCreateTriggerSql = (
  tableName: string,
  tableSchema: string,
  triggerName: string,
  trigger: {
    action_timing: string;
    event_manipulation: string;
    action_orientation: string;
    action_statement: string;
    comment: string;
  }
) => {
  let sql = `CREATE TRIGGER "${triggerName}"
${trigger.action_timing} ${trigger.event_manipulation} ON "${tableSchema}"."${tableName}"
FOR EACH ${trigger.action_orientation} ${trigger.action_statement};`;

  if (trigger.comment) {
    sql += `COMMENT ON TRIGGER "${triggerName}" ON "${tableSchema}"."${tableName}"
IS ${sqlEscapeText(trigger.comment)};`;
  }
  return sql;
};

export const getDropSql = (
  tableName: string,
  schemaName: string,
  property = 'table'
) => `DROP ${property} "${schemaName}"."${tableName}"`;

export const getViewDefinitionSql = (viewName: string) => `
  SELECT
    CASE WHEN pg_has_role(c.relowner, 'USAGE') THEN pg_get_viewdef(c.oid)
    ELSE null
    END AS view_definition,
    CASE WHEN c.relkind = 'v' THEN 'VIEW' ELSE 'MATERIALIZED VIEW' END AS view_type
  FROM pg_class c
  WHERE c.relname = '${viewName}'
    AND c.relkind in ('v', 'm')
    AND (pg_has_role(c.relowner, 'USAGE')
    OR has_table_privilege(c.oid, 'SELECT, INSERT, UPDATE, DELETE, TRUNCATE, REFERENCES, TRIGGER')
    OR has_any_column_privilege(c.oid, 'SELECT, INSERT, UPDATE, REFERENCES')
  )
`;

export const getDropColumnSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  options?: {
    // todo
    sqlGenerator?: FrequentlyUsedColumn['dependentSQLGenerator'];
  }
) => {
  let sql = `
  alter table "${schemaName}"."${tableName}" drop column "${columnName}" cascade
`;
  if (!options) {
    return sql;
  }

  if (options.sqlGenerator) {
    sql = `${
      options.sqlGenerator(schemaName, tableName, columnName).downSql
    } \n`;
  }

  sql += `alter table "${schemaName}"."${tableName}" drop column "${columnName}"`;

  return sql;
};

export const getAddColumnSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  columnType: string,
  options?: {
    nullable: boolean;
    unique: boolean;
    default: string;
    // todo
    sqlGenerator?: FrequentlyUsedColumn['dependentSQLGenerator'];
  }
) => {
  let sql = `
  alter table "${schemaName}"."${tableName}" add column "${columnName}" ${columnType}
`;
  if (!options) {
    return sql;
  }

  if (options.nullable) {
    sql += ' null';
  } else {
    sql += ' not null';
  }
  if (options.unique) {
    sql += ' unique';
  }
  if (options.default) {
    let defWithQuotes = '';

    if (isColTypeString(columnType) && !isSQLFunction(options.default)) {
      defWithQuotes = `'${options.default}'`;
    } else {
      defWithQuotes = options.default;
    }

    sql += ` default ${defWithQuotes}`;
  }

  sql += ';';

  if (options.sqlGenerator) {
    sql += '\n';
    sql += options.sqlGenerator(schemaName, tableName, columnName).upSql;
  }

  if (columnType === 'uuid' && options.default !== '') {
    return ['CREATE EXTENSION IF NOT EXISTS pgcrypto;', sql];
  }

  return sql;
};

export const getDropNullSql = (
  tableName: string,
  schemaName: string,
  columnName: string
) => `
  alter table "${schemaName}"."${tableName}" alter column "${columnName}" drop not null"
`;

export const getSetNullSql = (
  tableName: string,
  schemaName: string,
  columnName: string
) => `
  alter table "${schemaName}"."${tableName}" alter column "${columnName}" set not null"
`;

export const getAddUniqueConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string,
  columns: string[]
) => `
  alter table "${schemaName}"."${tableName}" add constraint "${constraintName}" unique (${columns.join(
  ', '
)})"
`;

export const getSetColumnDefaultSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  defaultValue: any,
  columnType: string
) => {
  let defWithQuotes = '';

  if (isColTypeString(columnType) && !isSQLFunction(defaultValue)) {
    defWithQuotes = `'${defaultValue}'`;
  } else {
    defWithQuotes = defaultValue;
  }
  const sql = `
  alter table "${schemaName}"."${tableName}" alter column "${columnName}" set default ${defWithQuotes}
`;
  return sql;
};

export const getSetCommentSql = (
  on: 'column' | 'table' | string,
  tableName: string,
  schemaName: string,
  columnName: string,
  comment: string | null
) => `
  comment on ${on} "${schemaName}"."${tableName}"."${columnName}" is ${
  comment ? sqlEscapeText(comment) : 'NULL'
}
`;

export const getAlterColumnTypeSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  columnType: string
) => `
  alter table "${schemaName}"."${tableName}" alter column "${columnName}" ${columnType}
`;

export const getDropColumnDefaultSql = (
  tableName: string,
  schemaName: string,
  columnName: string
) => `
  alter table "${schemaName}"."${tableName}" alter column "${columnName}" drop default
`;

export const getRenameColumnQuery = (
  tableName: string,
  schemaName: string,
  newName: string,
  oldName: string
) => `
  alter table "${schemaName}"."${tableName}" rename column "${oldName}" to "${newName}"
`;

/*
example result
{
  Source Type: bool
  Source Info: boolean,
  Source Description: boolean, 'true'/'false'
  Target Type: text,bpchar,varchar
  Target Info: text,character,character varying
  Target Descriptions: variable-length string, no limit specified:char(length), blank-padded string, fixed storage length:varchar(length), non-blank-padded string, variable storage length
  Function: text,text,text
}
*/
export const fetchColumnCastsQuery = `
SELECT ts.typname AS "Source Type",
       pg_catalog.format_type(castsource, NULL) AS "Source Info",
       coalesce(pg_catalog.obj_description(castsource, 'pg_type'), '') as "Source Descriptions",
       string_agg(tt.typname, ',') AS "Target Type",
       string_agg(pg_catalog.format_type(casttarget, NULL), ',') AS "Target Info",
       string_agg(coalesce(pg_catalog.obj_description(casttarget, 'pg_type'), ''), ':') as "Target Descriptions",
       string_agg(CASE WHEN castfunc = 0 THEN '(binary coercible)'
            ELSE p.proname
       END, ',') as "Function"
     FROM pg_catalog.pg_cast c LEFT JOIN pg_catalog.pg_proc p
     ON c.castfunc = p.oid
     LEFT JOIN pg_catalog.pg_type ts
     ON c.castsource = ts.oid
     LEFT JOIN pg_catalog.pg_namespace ns
     ON ns.oid = ts.typnamespace
     LEFT JOIN pg_catalog.pg_type tt
     ON c.casttarget = tt.oid
     LEFT JOIN pg_catalog.pg_namespace nt
     ON nt.oid = tt.typnamespace
WHERE ( (true  AND pg_catalog.pg_type_is_visible(ts.oid)
) OR (true  AND pg_catalog.pg_type_is_visible(tt.oid)
) ) AND (c.castcontext != 'e') AND ts.typname != tt.typname
GROUP BY ts.typname, castsource
ORDER BY 1, 2;
`;

export const checkSchemaModification = (sql: string) => {
  const sqlStatements = sql
    .toLowerCase()
    .split(';')
    .map(sqlStr => sqlStr.trim());

  return sqlStatements.some(
    statement =>
      statement.startsWith('create ') ||
      statement.startsWith('alter ') ||
      statement.startsWith('drop ')
  );
};

export const getCreateCheckConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string,
  check: string
) => {
  return `alter table "${schemaName}"."${tableName}" add constraint "${constraintName}" check (${check})`;
};

export const getCreatePkSql = ({
  schemaName,
  tableName,
  selectedPkColumns,
  constraintName,
}: {
  schemaName: string;
  tableName: string;
  selectedPkColumns: string[];
  constraintName: string;
}) => {
  return `alter table "${schemaName}"."${tableName}"
    add constraint "${constraintName}" 
    primary key ( ${selectedPkColumns.map(pkc => `"${pkc}"`).join(', ')} );`;
};
