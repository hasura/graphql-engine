import { SchemaResponse, ScalarType, ColumnInfo, TableInfo } from "./types"
import { Config } from "./config";
import { connect } from './db';

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
  const pk  = pks.length > 0 ? { primary_key: pks } : {};

  // TODO: Should we include something for the description here?
  return {
    name: [info.name],
    ...pk,
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

function ddlPKs(ddl: any): Array<any> {
  if(ddl.type != 'statement' || ddl.variant != 'list') {
    throw new Error("Encountered a non-statement or non-list when parsing DDL for table.");
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

export async function getSchema(config: Config): Promise<SchemaResponse> {
  const db                                        = connect(config);
  const [results, metadata]                       = await db.query("SELECT * from sqlite_schema");
  const resultsT: Array<TableInfoInternal>        = results as unknown as Array<TableInfoInternal>;
  const filtered: Array<TableInfoInternal>        = resultsT.filter(table => includeTable(config,table));
  const result:   Array<TableInfo>                = filtered.map(formatTableInfo);

  return {
    tables: result
  };
};
