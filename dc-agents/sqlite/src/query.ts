import { Config }  from "./config";
import { connect, SqlLogger } from "./db";
import { coerceUndefinedToNull, omap, last, coerceUndefinedOrNullToEmptyRecord, envToBool, isEmptyObject, tableNameEquals, unreachable, logDeep } from "./util";
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
    OrderDirection,
    UnaryComparisonOperator,
    ExplainResponse,
    ExistsExpression,
  } from "@hasura/dc-api-types";
import { customAlphabet } from "nanoid";

const SqlString = require('sqlstring-sqlite');

const nanoid = customAlphabet("abcdefghijklmnopqrstuvwxyz0123456789-_", 6);

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

function json_object(relationships: Array<TableRelationships>, fields: Fields, table: TableName): string {
  const result = omap(fields, (fieldName, field) => {
    switch(field.type) {
      case "column":
        return `${escapeString(fieldName)}, ${escapeIdentifier(field.column)}`;
      case "relationship":
        const tableRelationships = relationships.find(tr => tableNameEquals(tr.source_table)(table));
        if (tableRelationships === undefined) {
          throw new Error(`Couldn't find table relationships for table ${table}`);
        }
        const rel = tableRelationships.relationships[field.relationship];
        if(rel === undefined) {
          throw new Error(`Couldn't find relationship ${field.relationship} for field ${fieldName} on table ${table}`);
        }
        return `'${fieldName}', ${relationship(relationships, rel, field, table)}`;
      default:
        return unreachable(field["type"]);
    }
  }).join(", ");

  return tag('json_object', `JSON_OBJECT(${result})`);
}

function where_clause(relationships: Array<TableRelationships>, expression: Expression, queryTableName: TableName): string {
  // The query table doesn't have an alias, so we refer to it by name directly
  const queryTableAlias = escapeTableName(queryTableName);

  const generateWhere = (expression: Expression, currentTableName: TableName, currentTableAlias: string): string => {
    switch(expression.type) {
      case "not":
        const aNot = generateWhere(expression.expression, currentTableName, currentTableAlias);
          return `(NOT ${aNot})`;

      case "and":
        const aAnd = expression.expressions.flatMap(x => generateWhere(x, currentTableName, currentTableAlias));
        return aAnd.length > 0
          ? `(${aAnd.join(" AND ")})`
          : "(1 = 1)" // true

      case "or":
        const aOr = expression.expressions.flatMap(x => generateWhere(x, currentTableName, currentTableAlias));
        return aOr.length > 0
          ? `(${aOr.join(" OR ")})`
          : "(1 = 0)" // false

      case "exists":
        const joinInfo = calculateExistsJoinInfo(relationships, expression, currentTableName, currentTableAlias);
        const subqueryWhere = generateWhere(expression.where, joinInfo.joinTableName, joinInfo.joinTableAlias);
        const whereComparisons = [...joinInfo.joinComparisonFragments, subqueryWhere].join(" AND ");
        return tag('exists',`EXISTS (SELECT 1 FROM ${escapeTableName(joinInfo.joinTableName)} AS ${joinInfo.joinTableAlias} WHERE ${whereComparisons})`);

      case "unary_op":
        const uop = uop_op(expression.operator);
        const columnFragment = generateComparisonColumnFragment(expression.column, queryTableAlias, currentTableAlias);
        return `(${columnFragment} ${uop})`;

      case "binary_op":
        const bopLhs = generateComparisonColumnFragment(expression.column, queryTableAlias, currentTableAlias);
        const bop = bop_op(expression.operator);
        const bopRhs = generateComparisonValueFragment(expression.value, queryTableAlias, currentTableAlias);
        return `${bopLhs} ${bop} ${bopRhs}`;

      case "binary_arr_op":
        const bopALhs = generateComparisonColumnFragment(expression.column, queryTableAlias, currentTableAlias);
        const bopA = bop_array(expression.operator);
        const bopARhsValues = expression.values.map(v => escapeString(v)).join(", ");
        return `(${bopALhs} ${bopA} (${bopARhsValues}))`;

      default:
        return unreachable(expression['type']);
    }
  };

  return generateWhere(expression, queryTableName, queryTableAlias);
}

type ExistsJoinInfo = {
  joinTableName: TableName,
  joinTableAlias: string,
  joinComparisonFragments: string[]
}

function calculateExistsJoinInfo(allTableRelationships: Array<TableRelationships>, exists: ExistsExpression, sourceTableName: TableName, sourceTableAlias: string): ExistsJoinInfo {
  switch (exists.in_table.type) {
    case "related":
      const tableRelationships = find_table_relationship(allTableRelationships, sourceTableName);
      const relationship = tableRelationships.relationships[exists.in_table.relationship];
      const joinTableAlias = generateIdentifierAlias(extractRawTableName(relationship.target_table));

      const joinComparisonFragments = omap(
        relationship.column_mapping,
        (sourceColumnName, targetColumnName) =>
          `${sourceTableAlias}.${escapeIdentifier(sourceColumnName)} = ${joinTableAlias}.${escapeIdentifier(targetColumnName)}`
        );

      return {
        joinTableName: relationship.target_table,
        joinTableAlias,
        joinComparisonFragments,
      };

    case "unrelated":
      return {
        joinTableName: exists.in_table.table,
        joinTableAlias: generateIdentifierAlias(extractRawTableName(exists.in_table.table)),
        joinComparisonFragments: []
      };

    default:
      return unreachable(exists.in_table["type"]);
  }
}

function generateComparisonColumnFragment(comparisonColumn: ComparisonColumn, queryTableAlias: string, currentTableAlias: string): string {
  const path = comparisonColumn.path ?? [];
  if (path.length === 0) {
    return `${currentTableAlias}.${escapeIdentifier(comparisonColumn.name)}`
  } else if (path.length === 1 && path[0] === "$") {
    return `${queryTableAlias}.${escapeIdentifier(comparisonColumn.name)}`
  } else {
    throw new Error(`Unsupported path on ComparisonColumn: ${[...path, comparisonColumn.name].join(".")}`);
  }
}

function generateComparisonValueFragment(comparisonValue: ComparisonValue, queryTableAlias: string, currentTableAlias: string): string {
  switch (comparisonValue.type) {
    case "column":
      return generateComparisonColumnFragment(comparisonValue.column, queryTableAlias, currentTableAlias);
    case "scalar":
      return escapeString(comparisonValue.value);
    default:
      return unreachable(comparisonValue["type"]);
  }
}

function generateIdentifierAlias(identifier: string): string {
  const randomSuffix = nanoid();
  return escapeIdentifier(`${identifier}_${randomSuffix}`);
}

/**
 *
 * @param ts Array of Table Relationships
 * @param t Table Name
 * @returns Relationships matching table-name
 */
function find_table_relationship(ts: Array<TableRelationships>, t: TableName): TableRelationships {
  for(var i = 0; i < ts.length; i++) {
    const r = ts[i];
    if(tableNameEquals(r.source_table)(t)) {
      return r;
    }
  }
  throw new Error(`Couldn't find relationship ${ts}, ${t.join(".")} - This shouldn't happen.`);
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
    wOrder: OrderBy | null,
  ): string {
    const innerFromClauses = `${where(ts, wWhere, wJoin, table)} ${order(wOrder)} ${limit(wLimit)} ${offset(wOffset)}`;
    const aggregateSelect  = aggregates_query(table, aggregates, innerFromClauses);
    const fieldSelect      = isEmptyObject(fields)     ? [] : [`'rows', JSON_GROUP_ARRAY(j)`];
    const fieldFrom        = isEmptyObject(fields)     ? '' : (() => {
      // NOTE: The order of table prefixes are currently assumed to be from "parent" to "child".
      // NOTE: The reuse of the 'j' identifier should be safe due to scoping. This is confirmed in testing.
      if(wOrder === null || wOrder.elements.length < 1) {
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
        coerceUndefinedToNull(field.query.order_by),
      ));
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

function uop_op(o: UnaryComparisonOperator): string {
  let result = o;
  switch(o) {
    case 'is_null':               result = "IS NULL"; break;
  }
  return tag('uop_op',result);
}

function orderDirection(orderDirection: OrderDirection): string {
  switch (orderDirection) {
    case "asc":
    case "desc":
      return orderDirection.toUpperCase();
    default:
      return unreachable(orderDirection);
  }
}

function order(orderBy: OrderBy | null): string {
  if (orderBy === null || orderBy.elements.length < 1) {
    return "";
  }

  const result =
    orderBy.elements
      .map(orderByElement => {
        if (orderByElement.target_path.length > 0 || orderByElement.target.type !== "column") {
          throw new Error("Unsupported OrderByElement. Relations and aggregates and not supported.");
        }
        return `${orderByElement.target.column} ${orderDirection(orderByElement.order_direction)}`;
      })
      .join(', ');

  return tag('order',`ORDER BY ${result}`);
}

/**
 * @param whereExpression Nested expression used in the associated where clause
 * @param joinArray Join clauses
 * @returns string representing the combined where clause
 */
function where(ts: Array<TableRelationships>, whereExpression: Expression | null, joinArray: Array<string>, t: TableName): string {
  const whereClause = whereExpression !== null ? [where_clause(ts, whereExpression, t)] : [];
  const clauses = [...whereClause, ...joinArray];
  return clauses.length < 1
    ? ""
    : tag('where',`WHERE ${clauses.join(" AND ")}`);
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
    coerceUndefinedToNull(request.query.order_by),
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
export async function queryData(config: Config, sqlLogger: SqlLogger, queryRequest: QueryRequest): Promise<QueryResponse> {
  const db = connect(config, sqlLogger); // TODO: Should this be cached?
  const q = query(queryRequest);
  const [result, metadata] = await db.query(q);

  return output(result);
}

/**
 *
 * Constructs a query as per the `POST /query` endpoint but prefixes it with `EXPLAIN QUERY PLAN` before execution.
 *
 * Formatted result lines are included under the `lines` field. An initial blank line is included to work around a display bug.
 *
 * NOTE: The Explain related items are included here since they are a small extension of Queries, and another module may be overkill.
 *
 * @param config
 * @param sqlLogger
 * @param queryRequest
 * @returns
 */
export async function explain(config: Config, sqlLogger: SqlLogger, queryRequest: QueryRequest): Promise<ExplainResponse> {
  const db = connect(config, sqlLogger);
  const q = query(queryRequest);
  const [result, metadata] = await db.query(`EXPLAIN QUERY PLAN ${q}`);
  return {
    query: q,
    lines: [ "", ...formatExplainLines(result as Array<AnalysisEntry>)]
  }
}

function formatExplainLines(items: Array<AnalysisEntry>): Array<string> {
  const lines = Object.fromEntries(items.map(x => [x.id, x]));
  function depth(x: number): number {
    if(x < 1) {
      return 0;
    }
    return 2 + depth(lines[x].parent);
  }
  return items.map(x => `${' '.repeat(depth(x.parent))}${x.detail}`)
}

type AnalysisEntry = {
  id: number,
  parent: number,
  detail: string
}
