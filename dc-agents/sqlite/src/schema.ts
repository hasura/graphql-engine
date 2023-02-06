import { SchemaResponse, ScalarType, ColumnInfo, TableInfo, Constraint } from "@hasura/dc-api-types"
import { ScalarTypeKey } from "./capabilities";
import { Config } from "./config";
import { connect, SqlLogger } from './db';
import { MUTATIONS } from "./environment";

var sqliteParser = require('sqlite-parser');

type TableInfoInternal = {
  name: string,
  type: string,
  tbl_name: string,
  rootpage: Number,
  sql: string
}

type Datatype = {
  affinity: string, // Sqlite affinity, lowercased
  variant: string, // Declared type, lowercased
}

// Note: Using ScalarTypeKey here instead of ScalarType to show that we have only used
//       the capability documented types, and that ScalarTypeKey is a subset of ScalarType
function determineScalarType(datatype: Datatype): ScalarTypeKey {
  switch (datatype.variant) {
    case "bool": return "bool";
    case "boolean": return "bool";
    case "datetime": return "DateTime";
  }
  switch (datatype.affinity) {
    case "integer": return "number";
    case "real": return "number";
    case "numeric": return "number";
    case "text": return "string";
    default:
      console.log(`Unknown SQLite column type: ${datatype.variant} (affinity: ${datatype.affinity}). Interpreting as string.`);
      return "string";
  }
}

function getColumns(ast: any[], primaryKeys: string[]) : ColumnInfo[] {
  return ast.map(c => {
    const isPrimaryKey = primaryKeys.includes(c.name);

    return {
      name: c.name,
      type: determineScalarType(c.datatype),
      nullable: nullableCast(c.definition),
      insertable: MUTATIONS,
      updatable: MUTATIONS && !isPrimaryKey,
    };
  })
}

function nullableCast(ds: any[]): boolean {
  for(var d of ds) {
    if(d.type === 'constraint' && d.variant == 'not null') {
      return false;
    }
  }
  return true;
}

const formatTableInfo = (config: Config) => (info: TableInfoInternal): TableInfo => {
  const tableName = config.explicit_main_schema ? ["main", info.name] : [info.name];
  const ast = sqliteParser(info.sql);
  const columnsDdl = getColumnsDdl(ast);
  const primaryKeys = ddlPKs(ast);
  const foreignKeys = ddlFKs(config, tableName, ast);
  const primaryKey = primaryKeys.length > 0 ? { primary_key: primaryKeys } : {};
  const foreignKey = foreignKeys.length > 0 ? { foreign_keys: Object.fromEntries(foreignKeys) } : {};

  return {
    name: tableName,
    type: "table",
    ...primaryKey,
    ...foreignKey,
    description: info.sql,
    columns: getColumns(columnsDdl, primaryKeys),
    insertable: MUTATIONS,
    updatable: MUTATIONS,
    deletable: MUTATIONS,
  }
}

/**
 * @param table
 * @returns true if the table is an SQLite meta table such as a sequence, index, etc.
 */
function isMeta(table : TableInfoInternal) {
  return table.type != 'table';
}

function includeTable(config: Config, table: TableInfoInternal): boolean {
  if(config.tables === null) {
    if(isMeta(table) && ! config.meta) {
      return false;
    }
    return true;
  } else {
    return config.tables.indexOf(table.name) >= 0
  }
}

/**
 * Pulls columns from the output of sqlite-parser.
 * Note that this doesn't check if duplicates are present and will emit them as many times as they are present.
 * This is done as an easy way to preserve order.
 *
 * @param ddl - The output of sqlite-parser
 * @returns - List of columns as present in the output of sqlite-parser.
 */
function getColumnsDdl(ddl: any): any[] {
  if(ddl.type != 'statement' || ddl.variant != 'list') {
    throw new Error("Encountered a non-statement or non-list when parsing DDL for table.");
  }
  return ddl.statement.flatMap((t: any) => {
    if(t.type !=  'statement' || t.variant != 'create' || t.format != 'table') {
      return [];
    }
    return t.definition.flatMap((c: any) => {
      if(c.type != 'definition' || c.variant != 'column') {
        return [];
      }
      return [c];
    });
  })
}

/**
 * Example:
 *
 * foreign_keys: {
 *   "ArtistId->Artist.ArtistId": {
 *     column_mapping: {
 *       "ArtistId": "ArtistId"
 *     },
 *     foreign_table: "Artist",
 *   }
 * }
 *
 * NOTE: We currently don't log if the structure of the DDL is unexpected, which could be the case for composite FKs, etc.
 * NOTE: There could be multiple paths between tables.
 * NOTE: Composite keys are not currently supported.
 *
 * @param ddl
 * @returns [name, FK constraint definition][]
 */
function ddlFKs(config: Config, tableName: Array<string>, ddl: any): [string, Constraint][]  {
  if(ddl.type != 'statement' || ddl.variant != 'list') {
    throw new Error("Encountered a non-statement or non-list DDL for table.");
  }
  return ddl.statement.flatMap((t: any) => {
    if(t.type !=  'statement' || t.variant != 'create' || t.format != 'table') {
      return [];
    }
    return t.definition.flatMap((c: any) => {
      if(c.type != 'definition' || c.variant != 'constraint'
          || c.definition.length != 1 || c.definition[0].type != 'constraint' || c.definition[0].variant != 'foreign key') {
        return [];
      }
      if(c.columns.length != 1) {
        return [];
      }

      const definition = c.definition[0];
      const sourceColumn = c.columns[0];

      if(sourceColumn.type != 'identifier' || sourceColumn.variant != 'column') {
        return [];
      }

      if(definition.references == null || definition.references.columns == null || definition.references.columns.length != 1) {
        return [];
      }

      const destinationColumn = definition.references.columns[0];
      const foreignTable = config.explicit_main_schema ? ["main", definition.references.name] : [definition.references.name];
      return [[
        `${tableName.join('.')}.${sourceColumn.name}->${definition.references.name}.${destinationColumn.name}`,
        { foreign_table: foreignTable,
          column_mapping: {
            [sourceColumn.name]: destinationColumn.name
          }
        }
      ]];
    });
  })
}

function ddlPKs(ddl: any): string[] {
  if(ddl.type != 'statement' || ddl.variant != 'list') {
    throw new Error("Encountered a non-statement or non-list DDL for table.");
  }
  return ddl.statement.flatMap((t: any) => {
    if(t.type !=  'statement' || t.variant != 'create' || t.format != 'table') {
      return [];
    }
    return t.definition.flatMap((c: any) => {
      if(c.type != 'definition' || c.variant != 'constraint'
          || c.definition.length != 1 || c.definition[0].type != 'constraint' || c.definition[0].variant != 'primary key') {
        return [];
      }
      return c.columns.flatMap((x:any) => {
        if(x.type == 'identifier' && x.variant == 'column') {
          return [x.name];
        } else {
          return [];
        }
      });
    });
  })
}

export async function getSchema(config: Config, sqlLogger: SqlLogger): Promise<SchemaResponse> {
  const db                            = connect(config, sqlLogger);
  const [results, metadata]           = await db.query("SELECT * from sqlite_schema");
  const resultsT: TableInfoInternal[] = results as TableInfoInternal[];
  const filtered: TableInfoInternal[] = resultsT.filter(table => includeTable(config,table));
  const result:   TableInfo[]         = filtered.map(formatTableInfo(config));

  return {
    tables: result
  };
};
