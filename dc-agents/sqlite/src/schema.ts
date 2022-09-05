import { SchemaResponse, ScalarType, ColumnInfo, TableInfo, Constraint } from "@hasura/dc-api-types"
import { Config } from "./config";
import { connect, SqlLogger } from './db';
import { logDeep } from "./util";

var sqliteParser = require('sqlite-parser');

type TableInfoInternal = {
  name: string,
  type: string,
  tbl_name: string,
  rootpage: Number,
  sql: string
}

/**
 *
 * @param ColumnInfoInternalype as per HGE DataConnector IR
 * @returns SQLite's corresponding column type
 *
 * Note: This defaults to "string" when a type is not anticipated
 *       in order to be as permissive as possible but logs when
 *       this happens.
 */
function columnCast(ColumnInfoInternalype: string): ScalarType {
  switch(ColumnInfoInternalype) {
    case "string":
    case "number":
    case "bool":    return ColumnInfoInternalype as ScalarType;
    case "boolean": return "bool";
    case "numeric": return "number";
    case "integer": return "number";
    case "double":  return "number";
    case "float":   return "number";
    case "text":    return "string";
    default:
      console.log(`Unknown SQLite column type: ${ColumnInfoInternalype}. Interpreting as string.`)
      return "string";
  }
}

function getColumns(ast : Array<any>) : Array<ColumnInfo> {
  return ast.map(c => {
    return ({
      name: c.name,
      type: columnCast(datatypeCast(c.datatype)),
      nullable: nullableCast(c.definition)
    })
  })
}

// Interpret the sqlite-parser datatype as a schema column response type.
function datatypeCast(d: any): any {
  switch(d.variant) {
    case "datetime": return 'string';
    default: return d.affinity;
  }
}

function nullableCast(ds: Array<any>): boolean {
  for(var d of ds) {
    if(d.type === 'constraint' && d.variant == 'not null') {
      return false;
    }
  }
  return true;
}

function formatTableInfo(info : TableInfoInternal): TableInfo {
  const ast = sqliteParser(info.sql);
  const ddl = ddlColumns(ast);
  const pks = ddlPKs(ast);
  const fks = ddlFKs(ast);
  const pk  = pks.length > 0 ? { primary_key: pks } : {};
  const fk  = fks.length > 0 ? { foreign_keys: Object.fromEntries(fks) } : {};

  // TODO: Should we include something for the description here?
  return {
    name: [info.name],
    ...pk,
    ...fk,
    description: info.sql,
    columns: getColumns(ddl)
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
function ddlColumns(ddl: any): Array<any> {
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
 * @returns Array<[name, FK constraint definition]>
 */
function ddlFKs(ddl: any): Array<[string, Constraint]>  {
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

      return [[
        `${sourceColumn.name}->${definition.references.name}.${destinationColumn.name}`,
        { foreign_table: [definition.references.name],
          column_mapping: {
            [sourceColumn.name]: destinationColumn.name
          }
        }
      ]];
    });
  })
}

function ddlPKs(ddl: any): Array<string> {
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
  const db                                        = connect(config, sqlLogger);
  const [results, metadata]                       = await db.query("SELECT * from sqlite_schema");
  const resultsT: Array<TableInfoInternal>        = results as Array<TableInfoInternal>;
  const filtered: Array<TableInfoInternal>        = resultsT.filter(table => includeTable(config,table));
  const result:   Array<TableInfo>                = filtered.map(formatTableInfo);

  return {
    tables: result
  };
};
