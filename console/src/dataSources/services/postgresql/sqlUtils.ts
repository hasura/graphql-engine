import { Table, FrequentlyUsedColumn, IndexType } from '../../types';
import { isColTypeString } from '.';
import { FunctionState } from './types';
import { QualifiedTable } from '../../../metadata/types';
import { quoteDefault } from '../../../components/Services/Data/utils';

export const sqlEscapeText = (rawText: string) => {
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
    WITH partitions AS (
      SELECT array(
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
      ) AS names
    )
    SELECT
      pgn.nspname AS table_schema,
      pgc.relname AS table_name,
      CASE
        WHEN pgc.relkind = 'r' THEN 'TABLE'
        WHEN pgc.relkind = 'f' THEN 'FOREIGN TABLE'
        WHEN pgc.relkind = 'v' THEN 'VIEW'
        WHEN pgc.relkind = 'm' THEN 'MATERIALIZED VIEW'
        WHEN pgc.relkind = 'p' THEN 'PARTITIONED TABLE'
      END AS table_type,
      obj_description(pgc.oid) AS comment,
      COALESCE(json_agg(DISTINCT row_to_json(isc) :: jsonb || jsonb_build_object('comment', col_description(pga.attrelid, pga.attnum))) filter (WHERE isc.column_name IS NOT NULL), '[]' :: json) AS columns,
      COALESCE(json_agg(DISTINCT row_to_json(ist) :: jsonb || jsonb_build_object('comment', obj_description(pgt.oid))) filter (WHERE ist.trigger_name IS NOT NULL), '[]' :: json) AS triggers,
      row_to_json(isv) AS view_info
      FROM partitions, pg_class as pgc  
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

export const fetchColumnDefaultFunctions = (schema = 'public') => {
  let schemaList = `('pg_catalog', 'public'`;
  if (schema !== 'public') {
    schemaList += `, '${schema}'`;
  }
  schemaList += ')';
  return `
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
  AND (pgn.nspname IN ${schemaList})
  AND pgp.proretset=false
GROUP BY t.typname
ORDER BY t.typname ASC;`;
};

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
    pg_class c
  JOIN
    pg_namespace n ON c.relnamespace = n.oid
  WHERE
    c.relname = quote_ident('${tableName}') AND n.nspname = quote_ident('${schemaName}');
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

export type Col = {
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
    sqlCreateTable += `COMMENT ON TABLE "${currentSchema}"."${tableName}" IS ${sqlEscapeText(
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
  tableSchema: string,
  triggerName: string,
  tableName?: string // This arg has to be passed strictly
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
    comment?: string;
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
    nullable?: boolean;
    unique?: boolean;
    default?: any;
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
    const defWithQuotes = quoteDefault(options.default);
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

export const getDropNotNullSql = (
  tableName: string,
  schemaName: string,
  columnName: string
) => `
  alter table "${schemaName}"."${tableName}" alter column "${columnName}" drop not null
`;

export const getSetNotNullSql = (
  tableName: string,
  schemaName: string,
  columnName: string
) => `
  alter table "${schemaName}"."${tableName}" alter column "${columnName}" set not null
`;

export const getAddUniqueConstraintSql = (
  tableName: string,
  schemaName: string,
  constraintName: string,
  columns: string[]
) => `
  alter table "${schemaName}"."${tableName}" add constraint "${constraintName}" unique (${columns.join(
  ', '
)})
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
  comment: string | null,
  columnName?: string
) => {
  if (columnName) {
    return `
  comment on ${on} "${schemaName}"."${tableName}"."${columnName}" is ${
      comment ? sqlEscapeText(comment) : 'NULL'
    }
`;
  }

  return `
comment on ${on} "${schemaName}"."${tableName}" is ${
    comment ? sqlEscapeText(comment) : 'NULL'
  }
`;
};

export const getAlterColumnTypeSql = (
  tableName: string,
  schemaName: string,
  columnName: string,
  columnType: string
) => `
  ALTER TABLE "${schemaName}"."${tableName}" ALTER COLUMN "${columnName}" TYPE ${columnType};
`;

export const getDropColumnDefaultSql = (
  tableName: string,
  schemaName: string,
  columnName?: string
) => `
ALTER TABLE "${schemaName}"."${tableName}" ALTER COLUMN "${
  columnName ?? ''
}" drop default
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
  constraintName?: string; // compulsory for PG
}) => {
  return `alter table "${schemaName}"."${tableName}"
    add constraint "${constraintName}"
    primary key (${selectedPkColumns.map(pkc => `"${pkc}"`).join(', ')});`;
};
export const getAlterPkSql = ({
  schemaName,
  tableName,
  selectedPkColumns,
  constraintName,
}: {
  schemaName: string;
  tableName: string;
  selectedPkColumns: string[];
  constraintName: string; // compulsory for PG
}) => {
  return `BEGIN TRANSACTION;
ALTER TABLE "${schemaName}"."${tableName}" DROP CONSTRAINT "${constraintName}";

ALTER TABLE "${schemaName}"."${tableName}"
    ADD CONSTRAINT "${constraintName}" PRIMARY KEY (${selectedPkColumns
    .map(pkc => `"${pkc}"`)
    .join(', ')});
COMMIT TRANSACTION;`;
};

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
LEFT JOIN pg_description pd ON p.oid = pd.objoid
WHERE
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

export const primaryKeysInfoSql = (options: {
  schemas: string[];
  tables: Table[];
}) => `
SELECT
COALESCE(
  json_agg(
    row_to_json(info)
  ),
  '[]' :: JSON
)
FROM (
SELECT
tc.table_schema,
tc.table_name,
tc.constraint_name,
json_agg(constraint_column_usage.column_name) AS columns
FROM
information_schema.table_constraints tc
JOIN (
  SELECT
    x.tblschema AS table_schema,
    x.tblname AS table_name,
    x.colname AS column_name,
    x.cstrname AS constraint_name
  FROM ( SELECT DISTINCT
      nr.nspname,
      r.relname,
      a.attname,
      c.conname
    FROM
      pg_namespace nr,
      pg_class r,
      pg_attribute a,
      pg_depend d,
      pg_namespace nc,
      pg_constraint c
    WHERE
      nr.oid = r.relnamespace
      AND r.oid = a.attrelid
      AND d.refclassid = 'pg_class'::regclass::oid
      AND d.refobjid = r.oid
      AND d.refobjsubid = a.attnum
      AND d.classid = 'pg_constraint'::regclass::oid
      AND d.objid = c.oid
      AND c.connamespace = nc.oid
      AND c.contype = 'c'::"char"
      AND(r.relkind = ANY (ARRAY ['r'::"char", 'p'::"char"]))
      AND NOT a.attisdropped
    UNION ALL
    SELECT
      nr.nspname,
      r.relname,
      a.attname,
      c.conname
    FROM
      pg_namespace nr,
      pg_class r,
      pg_attribute a,
      pg_namespace nc,
      pg_constraint c
    WHERE
      nr.oid = r.relnamespace
      AND r.oid = a.attrelid
      AND nc.oid = c.connamespace
      AND r.oid = CASE c.contype
      WHEN 'f'::"char" THEN
        c.confrelid
      ELSE
        c.conrelid
      END
      AND(a.attnum = ANY (
          CASE c.contype
          WHEN 'f'::"char" THEN
            c.confkey
          ELSE
            c.conkey
          END))
      AND NOT a.attisdropped
      AND(c.contype = ANY (ARRAY ['p'::"char", 'u'::"char", 'f'::"char"]))
      AND(r.relkind = ANY (ARRAY ['r'::"char", 'p'::"char"]))) x (tblschema, tblname, colname, cstrname)) constraint_column_usage ON tc.constraint_name::text = constraint_column_usage.constraint_name::text
  AND tc.table_schema::text = constraint_column_usage.table_schema::text
  AND tc.table_name::text = constraint_column_usage.table_name::text
${generateWhereClause(options, 'tc.table_name', 'tc.table_schema')}
  AND tc.constraint_type::text = 'PRIMARY KEY'::text
GROUP BY
  tc.table_schema, tc.table_name, tc.constraint_name) as info;
`;

export const uniqueKeysSql = (options: {
  schemas: string[];
  tables: Table[];
}) => `
SELECT
COALESCE(
  json_agg(
    row_to_json(info)
  ),
  '[]' :: JSON
)
FROM (
	SELECT
		tc.table_name,
		tc.constraint_schema AS table_schema,
		tc.constraint_name,
		json_agg(kcu.column_name) AS columns
	FROM
		information_schema.table_constraints tc
		JOIN information_schema.key_column_usage kcu USING (constraint_schema, constraint_name)
    ${generateWhereClause(options, 'tc.table_name', 'tc.constraint_schema')}
		AND tc.constraint_type::text = 'UNIQUE'::text
	GROUP BY
		tc.table_name,
		tc.constraint_schema,
		tc.constraint_name) AS info;
`;

export const checkConstraintsSql = (options: {
  schemas: string[];
  tables: Table[];
}) => `
SELECT
COALESCE(
  json_agg(
    row_to_json(info)
  ),
  '[]' :: JSON
)
FROM (
SELECT n.nspname::text AS table_schema,
    ct.relname::text AS table_name,
    r.conname::text AS constraint_name,
    pg_get_constraintdef(r.oid, true) AS "check"
   FROM pg_constraint r
     JOIN pg_class ct ON r.conrelid = ct.oid
     JOIN pg_namespace n ON ct.relnamespace = n.oid
   ${generateWhereClause(options, 'ct.relname', 'n.nspname')}
   AND r.contype = 'c'::"char"
   ) AS info;
`;

export const tableIndexSql = (options: { schema: string; table: string }) => `
    SELECT
    COALESCE(
        json_agg(
            row_to_json(info)
        ),
        '[]' :: JSON
    ) AS indexes
    FROM
    (
      SELECT
          t.relname as table_name,
          i.relname as index_name,
          it.table_schema as table_schema,
          am.amname as index_type,
          array_agg(DISTINCT a.attname) as index_columns,
          pi.indexdef as index_definition_sql
      FROM
          pg_class t,
          pg_class i,
          pg_index ix,
          pg_attribute a,
          information_schema.tables it,
          pg_am am,
          pg_indexes pi
      WHERE
          t.oid = ix.indrelid
          and i.oid = ix.indexrelid
          and a.attrelid = t.oid
          and a.attnum = ANY(ix.indkey)
          and t.relkind = 'r'
          and pi.indexname = i.relname
          and t.relname = '${options.table}'
          and it.table_schema = '${options.schema}'
          and am.oid = i.relam
      GROUP BY
          t.relname,
          i.relname,
          it.table_schema,
          am.amname,
          pi.indexdef
      ORDER BY
          t.relname,
          i.relname
    ) as info;
  `;

export const getCreateIndexSql = (indexObj: {
  indexName: string;
  indexType: IndexType;
  table: QualifiedTable;
  columns: string[];
  unique?: boolean;
}) => {
  const { indexName, indexType, table, columns, unique = false } = indexObj;

  return `
  CREATE ${unique ? 'UNIQUE' : ''} INDEX "${indexName}" on
  "${table.schema}"."${table.name}" using ${indexType} (${columns
    .map(c => `"${c}"`)
    .join(', ')});
`;
};

export const getDropIndexSql = (indexName: string) =>
  `DROP INDEX IF EXISTS "${indexName}"`;

export const frequentlyUsedColumns: FrequentlyUsedColumn[] = [
  {
    name: 'id',
    validFor: ['add'],
    type: 'serial',
    typeText: 'integer (auto-increment)',
    primary: true,
  },
  {
    name: 'id',
    validFor: ['add'],
    type: 'bigserial',
    typeText: 'bigint (auto-increment)',
    primary: true,
  },
  {
    name: 'id',
    validFor: ['add'],
    type: 'int GENERATED BY DEFAULT AS IDENTITY',
    typeText: 'int (identity, generated by default)',
    primary: true,
    minPGVersion: 10,
  },
  {
    name: 'id',
    validFor: ['add'],
    type: 'uuid',
    typeText: 'UUID',
    primary: true,
    default: 'gen_random_uuid()',
  },
  {
    name: 'created_at',
    validFor: ['add', 'modify'],
    type: 'timestamptz',
    typeText: 'timestamp',
    default: 'now()',
  },
  {
    name: 'updated_at',
    validFor: ['add', 'modify'],
    type: 'timestamptz',
    typeText: 'timestamp',
    default: 'now()',
    defaultText: 'now() + trigger to set value on update',
    dependentSQLGenerator: (schemaName, tableName, columnName) => {
      const upSql = `
CREATE OR REPLACE FUNCTION "${schemaName}"."set_current_timestamp_${columnName}"()
RETURNS TRIGGER AS $$
DECLARE
  _new record;
BEGIN
  _new := NEW;
  _new."${columnName}" = NOW();
  RETURN _new;
END;
$$ LANGUAGE plpgsql;
CREATE TRIGGER "set_${schemaName}_${tableName}_${columnName}"
BEFORE UPDATE ON "${schemaName}"."${tableName}"
FOR EACH ROW
EXECUTE PROCEDURE "${schemaName}"."set_current_timestamp_${columnName}"();
COMMENT ON TRIGGER "set_${schemaName}_${tableName}_${columnName}" ON "${schemaName}"."${tableName}" 
IS 'trigger to set value of column "${columnName}" to current timestamp on row update';
`;

      const downSql = `DROP TRIGGER IF EXISTS "set_${schemaName}_${tableName}_${columnName}" ON "${schemaName}"."${tableName}";`;

      return {
        upSql,
        downSql,
      };
    },
  },
];

export const getFKRelations = (options: {
  schemas: string[];
  tables: Table[];
}) => `
SELECT
	COALESCE(json_agg(row_to_json(info)), '[]'::JSON)
FROM (
	SELECT
		q.table_schema::text AS table_schema,
		q.table_name::text AS table_name,
		q.constraint_name::text AS constraint_name,
		min(q.ref_table_table_schema::text) AS ref_table_table_schema,
		min(q.ref_table::text) AS ref_table,
		json_object_agg(ac.attname, afc.attname) AS column_mapping,
		min(q.confupdtype::text) AS on_update,
		min(q.confdeltype::text) AS
		on_delete
	FROM (
		SELECT
			ctn.nspname AS table_schema,
			ct.relname AS table_name,
			r.conrelid AS table_id,
			r.conname AS constraint_name,
			cftn.nspname AS ref_table_table_schema,
			cft.relname AS ref_table,
			r.confrelid AS ref_table_id,
			r.confupdtype,
			r.confdeltype,
			unnest(r.conkey) AS column_id,
			unnest(r.confkey) AS ref_column_id
		FROM
			pg_constraint r
			JOIN pg_class ct ON r.conrelid = ct.oid
			JOIN pg_namespace ctn ON ct.relnamespace = ctn.oid
			JOIN pg_class cft ON r.confrelid = cft.oid
			JOIN pg_namespace cftn ON cft.relnamespace = cftn.oid
    WHERE
      r.contype = 'f'::"char"
      ${generateWhereClause(options, 'ct.relname', 'ctn.nspname', 'AND')}
      ) q
		JOIN pg_attribute ac ON q.column_id = ac.attnum
			AND q.table_id = ac.attrelid
		JOIN pg_attribute afc ON q.ref_column_id = afc.attnum
			AND q.ref_table_id = afc.attrelid
		GROUP BY
			q.table_schema,
			q.table_name,
      q.constraint_name) AS info;`;

export const deleteFunctionSql = (
  schemaName: string,
  functionState: FunctionState
) => {
  const { functionName, inputArgTypes } = functionState;

  const functionNameWithSchema = `"${schemaName}"."${functionName}"`;

  let functionArgString = '';
  if (inputArgTypes.length > 0) {
    functionArgString += '(';
    inputArgTypes.forEach((inputArg, i) => {
      functionArgString += i > 0 ? ', ' : '';

      functionArgString += `"${inputArg.schema}"."${inputArg.name}"`;
    });
    functionArgString += ')';
  }

  return `DROP FUNCTION ${functionNameWithSchema}${functionArgString}`;
};

export const getEventInvocationInfoByIDSql = (
  logTableDef: QualifiedTable,
  eventLogTable: QualifiedTable,
  eventId: string
) => `
  SELECT
    original_table.*,
    event.*
  FROM
  "${logTableDef.schema}"."${logTableDef.name}" original_table
  JOIN "${eventLogTable.schema}"."${eventLogTable.name}" event ON original_table.event_id = event.id
  WHERE original_table.event_id = '${eventId}'
  ORDER BY original_table.created_at DESC NULLS LAST;
`;

/**
 * SQL to retrive:
 * { table_name: string, table_schema: string, columns: string[] }[].
 *
 * `columns` is an array of column names.
 */
export const getDatabaseInfo = `
SELECT
	COALESCE(json_agg(row_to_json(info)), '[]'::JSON)
FROM (
	SELECT
		table_name::text,
		table_schema::text,
		ARRAY_AGG("column_name"::text) as columns
	FROM
		information_schema.columns
	WHERE
		table_schema NOT in('information_schema', 'pg_catalog', 'hdb_catalog')
		AND table_schema NOT LIKE 'pg_toast%'
		AND table_schema NOT LIKE 'pg_temp_%'
	GROUP BY
		table_name,
		table_schema) AS info;
`;

export const getTableInfo = (tables: QualifiedTable[]) => `
SELECT
	COALESCE(json_agg(row_to_json(info)), '[]'::JSON)
FROM (
    select
        pgclass.relname::text as table_name,
        n.nspname as table_schema,
        CASE
        WHEN pgclass.relkind = 'v' THEN 'view'
        WHEN pgclass.relkind = 'r' THEN 'table'
        WHEN pgclass.relkind = 'm' THEN 'materialized_view'
        END as table_type
        from pg_class pgclass
        join pg_catalog.pg_namespace n
        on n.oid = pgclass.relnamespace
        where
        pgclass.relname in (${tables.map(t => `'${t.name}'`).join(',')})
) AS info;
`;

export const getDatabaseVersionSql = 'SELECT version();';
