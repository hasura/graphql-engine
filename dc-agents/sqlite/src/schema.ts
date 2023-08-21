import { SchemaResponse, ColumnInfo, TableInfo, Constraint, ColumnValueGenerationStrategy, SchemaRequest, DetailLevel, TableName } from "@hasura/dc-api-types"
import { ScalarTypeKey } from "./capabilities";
import { Config } from "./config";
import { defaultMode, SqlLogger, withConnection } from './db';
import { MUTATIONS } from "./environment";
import { unreachable } from "./util";

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

function getColumns(ast: any[]) : ColumnInfo[] {
  return ast.map(column => {
    const isAutoIncrement = column.definition.some((def: any) => def.type === "constraint" && def.autoIncrement === true);

    return {
      name: column.name,
      type: determineScalarType(column.datatype),
      nullable: nullableCast(column.definition),
      insertable: MUTATIONS,
      updatable: MUTATIONS,
      ...(isAutoIncrement ? { value_generated: { type: "auto_increment" } } : {})
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

const formatTableInfo = (config: Config, detailLevel: DetailLevel): ((info: TableInfoInternal) => TableInfo) => {
  switch (detailLevel) {
    case "everything": return formatEverythingTableInfo(config);
    case "basic_info": return formatBasicTableInfo(config);
    default: return unreachable(detailLevel);
  }
}

const formatBasicTableInfo = (config: Config) => (info: TableInfoInternal): TableInfo => {
  const tableName = config.explicit_main_schema ? ["main", info.name] : [info.name];
  return {
    name: tableName,
    type: "table"
  }
}

const formatEverythingTableInfo = (config: Config) => (info: TableInfoInternal): TableInfo => {
  const basicTableInfo = formatBasicTableInfo(config)(info);
  const ast = sqliteParser(info.sql);
  const columnsDdl = getColumnsDdl(ast);
  const primaryKeys = getPrimaryKeyNames(ast);
  const foreignKeys = ddlFKs(config, basicTableInfo.name, ast);
  const primaryKey = primaryKeys.length > 0 ? { primary_key: primaryKeys } : {};
  const foreignKey = foreignKeys.length > 0 ? { foreign_keys: Object.fromEntries(foreignKeys) } : {};

  return {
    ...basicTableInfo,
    ...primaryKey,
    ...foreignKey,
    description: info.sql,
    columns: getColumns(columnsDdl),
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
  return table.type != 'table' || table.name === 'sqlite_sequence';
}

const includeTable = (config: Config, only_tables?: TableName[]) => (table: TableInfoInternal): boolean => {
  if (isMeta(table) && !config.meta) {
    return false;
  }

  const filterForOnlyTheseTables = only_tables
    // If we're using an explicit main schema, only use those table names that belong to that schema
    ?.filter(n => config.explicit_main_schema ? n.length === 2 && n[0] === "main" : true)
    // Just keep the actual table name
    ?.map(n => n[n.length - 1])

  if (config.tables || only_tables) {
    return (config.tables ?? []).concat(filterForOnlyTheseTables ?? []).indexOf(table.name) >= 0;
  } else {
    return true;
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

function getPrimaryKeyNames(ddl: any): string[] {
  if(ddl.type != 'statement' || ddl.variant != 'list') {
    throw new Error("Encountered a non-statement or non-list DDL for table.");
  }

  return ddl.statement
    .filter((ddlStatement: any) => ddlStatement.type === 'statement' && ddlStatement.variant === 'create' && ddlStatement.format === 'table')
    .flatMap((createTableDef: any) => {
      // Try to discover PKs defined on the column
      // (eg 'Id INTEGER PRIMARY KEY NOT NULL')
      const pkColumns =
        createTableDef.definition
          .filter((def: any) => def.type === 'definition' && def.variant === 'column')
          .flatMap((columnDef: any) =>
            columnDef.definition.some((def: any) => def.type === 'constraint' && def.variant === 'primary key')
              ? [columnDef.name]
              : []
          );
      if (pkColumns.length > 0)
        return pkColumns;

      // Try to discover explicit PK constraint defined inside create table DDL
      // (eg 'CONSTRAINT [PK_Test] PRIMARY KEY ([Id])')
      const pkConstraintColumns =
        createTableDef.definition
          .filter((def: any) => def.type === 'definition' && def.variant === 'constraint' && def.definition.length === 1 && def.definition[0].type === 'constraint' && def.definition[0].variant === 'primary key')
          .flatMap((pkConstraintDef: any) =>
            pkConstraintDef.columns.flatMap((def: any) =>
              def.type === 'identifier' && def.variant === 'column'
                ? [def.name]
                : []
            )
          );

      return pkConstraintColumns;
    })
}

export async function getSchema(config: Config, sqlLogger: SqlLogger, schemaRequest: SchemaRequest = {}): Promise<SchemaResponse> {
  return await withConnection(config, defaultMode, sqlLogger, async db => {
    const detailLevel = schemaRequest.detail_level ?? "everything";

    const results = await db.query("SELECT * from sqlite_schema");
    const resultsT: TableInfoInternal[] = results as TableInfoInternal[];
    const filtered: TableInfoInternal[] = resultsT.filter(includeTable(config, schemaRequest?.filters?.only_tables));
    const result:   TableInfo[]         = filtered.map(formatTableInfo(config, detailLevel));

    return {
      tables: result
    };
  });
};
