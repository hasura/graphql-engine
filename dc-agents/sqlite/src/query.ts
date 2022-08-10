import { Config }  from "./config";
import { connect } from "./db";
import { coerceUndefinedOrNullToEmptyArray, coerceUndefinedToNull, omap, last, coerceUndefinedOrNullToEmptyRecord, envToBool, logDeep, isEmptyObject, tableNameEquals } from "./util";
import {
    Expression,
    BinaryComparisonOperator,
    ComparisonValue,
    QueryRequest,
    ComparisonColumn,
    TableRelationships,
    Relationship,
    RelationshipField,
    BinaryArrayComparisonOperator,
    OrderBy,
    QueryResponse,
    Field,
    Aggregate,
    TableName,
  } from "./types";

const SqlString = require('sqlstring-sqlite');

/** Helper type for convenience. Uses the sqlstring-sqlite library, but should ideally use the function in sequalize.
 */
type Fields = Record<string, Field>
type Aggregates = Record<string, Aggregate>

function escapeString(x: any): string {
  return SqlString.escape(x);
}

/**
 *
 * @param identifier: Unescaped name. E.g. 'Alb"um'
 * @returns Escaped name. E.g. '"Alb\"um"'
 */
function escapeIdentifier(identifier: string): string {
  // TODO: Review this function since the current implementation is off the cuff.
  const result = identifier.replace(/\\/g,"\\\\").replace(/"/g,'\\"');
  return `"${result}"`;
}

function extractRawTableName(tableName: TableName): string {
  if (tableName.length === 1)
    return tableName[0];
  else
    throw new Error(`${tableName.join(".")} is not a valid table`);
}

/**
 *
 * @param tableName: Unescaped table name. E.g. 'Alb"um'
 * @returns Escaped table name. E.g. '"Alb\"um"'
 */
 function escapeTableName(tableName: TableName): string {
  return escapeIdentifier(extractRawTableName(tableName));
}

function json_object(ts: Array<TableRelationships>, fields: Fields, table: TableName): string {
  const result = omap(fields, (k,v) => {
    switch(v.type) {
      case "column":
        return [`${escapeString(k)}, ${escapeIdentifier(v.column)}`];
      case "relationship":
        const result = ts.flatMap((x) => {
          if(tableNameEquals(x.source_table)(table)) {
            const rel = x.relationships[v.relationship];
            if(rel) {
              return [`'${k}', ${relationship(ts, rel, v, table)}`];
            }
          }
          return [];
        });
        if(result.length < 1) {
          console.log("Couldn't find relationship for field", k, v, ts);
        }
        return result;
    }
  }).flatMap((e) => e).join(", ");

  return tag('json_object', `JSON_OBJECT(${result})`);
}

function where_clause(ts: Array<TableRelationships>, w: Expression | null, t: TableName): Array<string> {
  if(w === null) {
    return [];
  } else {
    switch(w.type) {
      case "not":
        const aNot = where_clause(ts, w.expression, t);
        if(aNot.length > 0) {
          return [`(NOT ${aNot})`];
        }
        break;
      case "and":
        const aAnd = w.expressions.flatMap(x => where_clause(ts, x, t));
        if(aAnd.length > 0) {
          return [`(${aAnd.join(" AND ")})`];
        }
        break;
      case "or":
        const aOr = w.expressions.flatMap(x => where_clause(ts, x, t));
        if(aOr.length > 0) {
          return [`(${aOr.join(" OR ")})`];
        }
        break;
      case "unary_op":
        switch(w.operator) {
          case 'is_null':
            if(w.column.path.length < 1) {
              return [`(${escapeIdentifier(w.column.name)} IS NULL)`];
            } else {
              return [exists(ts, w.column, t, 'IS NULL')];
            }
          default:
            if(w.column.path.length < 1) {
              return [`(${escapeIdentifier(w.column.name)} ${w.operator})`];
            } else {
              return [exists(ts, w.column, t, w.operator)];
            }
        }
      case "binary_op":
        const bop = bop_op(w.operator);
        if(w.column.path.length < 1) {
          return [`${escapeIdentifier(w.column.name)} ${bop} ${bop_val(w.value, t)}`];
        } else {
          return [exists(ts, w.column, t, `${bop} ${bop_val(w.value, t)}`)];
        }
      case "binary_arr_op":
        const bopA = bop_array(w.operator);
        if(w.column.path.length < 1) {
          return [`(${escapeIdentifier(w.column.name)} ${bopA} (${w.values.map(v => escapeString(v)).join(", ")}))`];
        } else {
          return [exists(ts,w.column,t, `${bopA} (${w.values.map(v => escapeString(v)).join(", ")})`)];
        }
    }
    return [];
  }
}

function exists(ts: Array<TableRelationships>, c: ComparisonColumn, t: TableName, o: string): string {
  // NOTE: An N suffix doesn't guarantee that conflicts are avoided.
  const r = join_path(ts, t, c.path, 0);
  const f = `FROM ${r.f.map(x => `${x.from} AS ${x.as}`).join(', ')}`;
  return tag('exists',`EXISTS (SELECT 1 ${f} WHERE ${[...r.j, `${last(r.f).as}.${escapeIdentifier(c.name)} ${o}`].join(' AND ')})`);
}

/** Develops a from clause for an operation with a path - a relationship referenced column
 *
 * Artist [Albums] Title
 * FROM Album Album_PATH_XX ...
 * WHERE Album_PATH_XX.ArtistId = Artist.ArtistId
 * Album_PATH_XX.Title IS NULL
 *
 * @param ts
 * @param table
 * @param path
 * @returns the from clause for the EXISTS query
 */
function join_path(ts: TableRelationships[], table: TableName, path: Array<string>, level: number): {f: Array<{from: string, as: string}>, j: string[]} {
  const r = find_table_relationship(ts, table);
  if(path.length < 1) {
    return {f: [], j: []};
  } else if(r === null) {
    throw new Error(`Couldn't find relationship ${ts}, ${table.join(".")} - This shouldn't happen.`);
  } else {
    const x = r.relationships[path[0]];
    const n = join_path(ts, x.target_table, path.slice(1), level+1);
    const m =
      omap(
        x.column_mapping,
        (sourceColumnName,targetColumnName) =>
          `${depthQualifyIdentifier(level-1,extractRawTableName(table))}.${escapeIdentifier(sourceColumnName)} = ${depthQualifyIdentifier(level, extractRawTableName(x.target_table))}.${escapeIdentifier(targetColumnName)}`
      )
      .join(' AND ');
    return {f: [{from: escapeTableName(x.target_table), as: depthQualifyIdentifier(level, extractRawTableName(x.target_table))}, ...n.f], j: [m, ...n.j]};
  }
}

function depthQualifyIdentifier(depth: number, identifier:string): string {
  if(depth < 0) {
    return escapeIdentifier(identifier);
  } else {
    return escapeIdentifier(`${identifier}_${depth}`);
  }
}

/**
 *
 * @param ts Array of Table Relationships
 * @param t Table Name
 * @returns Relationships matching table-name
 */
function find_table_relationship(ts: Array<TableRelationships>, t: TableName): TableRelationships | null {
  for(var i = 0; i < ts.length; i++) {
    const r = ts[i];
    if(tableNameEquals(r.source_table)(t)) {
      return r;
    }
  }
  return null;
}

function cast_aggregate_function(f: string): string {
  switch(f) {
    case 'avg':
    case 'max':
    case 'min':
    case 'sum':
    case 'total':
      return f;
    default:
      throw new Error(`Aggregate function ${f} is not supported by SQLite. See: https://www.sqlite.org/lang_aggfunc.html`);
  }
}

/**
 * Builds an Aggregate query expression.
 *
 * NOTE: ORDER Clauses are currently broken due to SQLite parser issue.
 *
 * @param table
 * @param aggregates
 * @param innerFromClauses
 * @returns
 */
function aggregates_query(
    table: TableName,
    aggregates: Aggregates,
    innerFromClauses: string,
  ): Array<string> {
    if(isEmptyObject(aggregates)) {
      return [];
    } else {
      const aggregate_pairs = omap(aggregates, (k,v) => {
        switch(v.type) {
          case 'star_count':
            return `${escapeString(k)}, COUNT(*)`;
          case 'column_count':
            if(v.distinct) {
              return `${escapeString(k)}, COUNT(DISTINCT ${escapeIdentifier(v.column)})`;
            } else {
              return `${escapeString(k)}, COUNT(${escapeIdentifier(v.column)})`;
            }
          case 'single_column':
            return `${escapeString(k)}, ${cast_aggregate_function(v.function)}(${escapeIdentifier(v.column)})`;
        }
      }).join(', ');

      return [`'aggregates', (SELECT JSON_OBJECT(${aggregate_pairs}) FROM (SELECT * from ${escapeTableName(table)} ${innerFromClauses}))`]
    }
}

function array_relationship(
    ts: Array<TableRelationships>,
    table: TableName,
    wJoin: Array<string>,
    fields: Fields,
    aggregates: Aggregates,
    wWhere: Expression | null,
    wLimit: number | null,
    wOffset: number | null,
    wOrder: Array<OrderBy>,
  ): string {
    const innerFromClauses = `${where(ts, wWhere, wJoin, table)} ${order(wOrder)} ${limit(wLimit)} ${offset(wOffset)}`;
    const aggregateSelect  = aggregates_query(table, aggregates, innerFromClauses);
    const fieldSelect      = isEmptyObject(fields)     ? [] : [`'rows', JSON_GROUP_ARRAY(j)`];
    const fieldFrom        = isEmptyObject(fields)     ? '' : (() => {
      // NOTE: The order of table prefixes are currently assumed to be from "parent" to "child".
      // NOTE: The reuse of the 'j' identifier should be safe due to scoping. This is confirmed in testing.
      if(wOrder.length < 1) {
        return `FROM ( SELECT ${json_object(ts, fields, table)} AS j FROM ${escapeTableName(table)} ${innerFromClauses})`;
      } else {
        const innerSelect = `SELECT * FROM ${escapeTableName(table)} ${innerFromClauses}`;
        return `FROM (SELECT ${json_object(ts, fields, table)} AS j FROM (${innerSelect}) AS ${table})`;
      }
    })()

    return tag('array_relationship',`(SELECT JSON_OBJECT(${[...fieldSelect, ...aggregateSelect].join(', ')}) ${fieldFrom})`);
}

function object_relationship(
    ts: Array<TableRelationships>,
    table: TableName,
    wJoin: Array<string>,
    fields: Fields,
  ): string {
      // NOTE: The order of table prefixes are from "parent" to "child".
      const innerFrom = `${escapeTableName(table)} ${where(ts, null, wJoin, table)}`;
      return tag('object_relationship',
        `(SELECT JSON_OBJECT('rows', JSON_ARRAY(${json_object(ts, fields, table)})) AS j FROM ${innerFrom})`);
}

function relationship(ts: Array<TableRelationships>, r: Relationship, field: RelationshipField, table: TableName): string {
  const wJoin = omap(
    r.column_mapping,
    (k,v) => `${escapeTableName(table)}.${escapeIdentifier(k)} = ${escapeTableName(r.target_table)}.${escapeIdentifier(v)}`
  );

  switch(r.relationship_type) {
    case 'object':
      return tag('relationship', object_relationship(
        ts,
        r.target_table,
        wJoin,
        coerceUndefinedOrNullToEmptyRecord(field.query.fields),
      ));

    case 'array':
      return tag('relationship', array_relationship(
        ts,
        r.target_table,
        wJoin,
        coerceUndefinedOrNullToEmptyRecord(field.query.fields),
        coerceUndefinedOrNullToEmptyRecord(field.query.aggregates),
        coerceUndefinedToNull(field.query.where),
        coerceUndefinedToNull(field.query.limit),
        coerceUndefinedToNull(field.query.offset),
        coerceUndefinedOrNullToEmptyArray(field.query.order_by),
      ));
  }
}

// TODO: There is a bug in this implementation where vals can reference columns with paths.
function bop_col(c: ComparisonColumn, t: TableName): string {
  if(c.path.length < 1) {
    return tag('bop_col', `${escapeTableName(t)}.${escapeIdentifier(c.name)}`);
  } else {
    throw new Error(`bop_col shouldn't be handling paths.`);
  }
}

function bop_array(o: BinaryArrayComparisonOperator): string {
  switch(o) {
    case 'in': return tag('bop_array','IN');
    default: return tag('bop_array', o);
  }
}

function bop_op(o: BinaryComparisonOperator): string {
  let result = o;
  switch(o) {
    case 'equal':                 result = "="; break;
    case 'greater_than':          result = ">"; break;
    case 'greater_than_or_equal': result = ">="; break;
    case 'less_than':             result = "<"; break;
    case 'less_than_or_equal':    result = "<="; break;
  }
  return tag('bop_op',result);
}

function bop_val(v: ComparisonValue, t: TableName): string {
  switch(v.type) {
    case "column": return tag('bop_val', bop_col(v.column, t));
    case "scalar": return tag('bop_val', escapeString(v.value));
  }
}

function order(o: Array<OrderBy>): string {
  if(o.length < 1) {
    return "";
  }
  const result = o.map(e => `${e.column} ${e.ordering}`).join(', ');
  return tag('order',`ORDER BY ${result}`);
}

/**
 * @param whereExpression Nested expression used in the associated where clause
 * @param joinArray Join clauses
 * @returns string representing the combined where clause
 */
function where(ts: Array<TableRelationships>, whereExpression: Expression | null, joinArray: Array<string>, t: TableName): string {
  const clauses = [...where_clause(ts, whereExpression, t), ...joinArray];
  if(clauses.length < 1) {
    return "";
  } else {
    return tag('where',`WHERE ${clauses.join(" AND ")}`);
  }
}

function limit(l: number | null): string {
  if(l === null) {
    return "";
  } else {
    return tag('limit',`LIMIT ${l}`);
  }
}

function offset(o: number | null): string {
  if(o === null) {
    return "";
  } else {
    return tag('offset', `OFFSET ${o}`);
  }
}

/** Top-Level Query Function.
 */
function query(request: QueryRequest): string {
  const result = array_relationship(
    request.table_relationships,
    request.table,
    [],
    coerceUndefinedOrNullToEmptyRecord(request.query.fields),
    coerceUndefinedOrNullToEmptyRecord(request.query.aggregates),
    coerceUndefinedToNull(request.query.where),
    coerceUndefinedToNull(request.query.limit),
    coerceUndefinedToNull(request.query.offset),
    coerceUndefinedOrNullToEmptyArray(request.query.order_by),
    );
  return tag('query', `SELECT ${result} as data`);
}

/** Format the DB response into a /query response.
 *
 * Note: There should always be one result since 0 rows still generates an empty JSON array.
 */
function output(rows: any): QueryResponse {
  return JSON.parse(rows[0].data);
}

const DEBUGGING_TAGS = envToBool('DEBUGGING_TAGS');
/** Function to add SQL comments to the generated SQL to tag which procedures generated what text.
 *
 * comment('a','b') => '/*\<a>\*\/ b /*\</a>*\/'
 */
function tag(t: string, s: string): string {
  if(DEBUGGING_TAGS) {
    return `/*<${t}>*/ ${s} /*</${t}>*/`;
  } else {
    return s;
  }
}

/** Performs a query and returns results
 *
 * Limitations:
 *
 * - Binary Array Operations not currently supported.
 *
 * The current algorithm is to first create a query, then execute it, returning results.
 *
 * Method for adding relationship fields:
 *
 * - JSON aggregation similar to Postgres' approach.
 *     - 4.13. The json_group_array() and json_group_object() aggregate SQL functions
 *     - https://www.sqlite.org/json1.html#jgrouparray
 *


 * Example of a test query:
 *
 * ```
 * query MyQuery {
 *   Artist(limit: 5, order_by: {ArtistId: asc}, where: {Name: {_neq: "Accept"}, _and: {Name: {_is_null: false}}}, offset: 3) {
 *     ArtistId
 *     Name
 *     Albums(where: {Title: {_is_null: false, _gt: "A", _nin: "foo"}}, limit: 2) {
 *       AlbumId
 *       Title
 *       ArtistId
 *       Tracks(limit: 1) {
 *         Name
 *         TrackId
 *       }
 *       Artist {
 *         ArtistId
 *       }
 *     }
 *   }
 *   Track(limit: 3) {
 *     Name
 *     Album {
 *       Title
 *     }
 *   }
 * }
 * ```
 *
 */
export async function queryData(config: Config, queryRequest: QueryRequest): Promise<QueryResponse> {
  const db = connect(config); // TODO: Should this be cached?
  const q = query(queryRequest);
  const [result, metadata] = await db.query(q);

  return output(result);
}
