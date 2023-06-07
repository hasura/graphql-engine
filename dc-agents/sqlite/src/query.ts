import { Config }  from "./config";
import { defaultMode, SqlLogger, withConnection } from "./db";
import { coerceUndefinedToNull, coerceUndefinedOrNullToEmptyRecord, isEmptyObject, tableNameEquals, unreachable, stringArrayEquals, ErrorWithStatusCode, mapObject } from "./util";
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
    OrderByRelation,
    OrderByElement,
    OrderByTarget,
    ScalarValue,
    TableRequest,
    FunctionRelationships,
  } from "@hasura/dc-api-types";
import { customAlphabet } from "nanoid";
import { DEBUGGING_TAGS, QUERY_LENGTH_LIMIT } from "./environment";

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
export function escapeIdentifier(identifier: string): string {
  // TODO: Review this function since the current implementation is off the cuff.
  const result = identifier.replace(/\\/g,"\\\\").replace(/"/g,'\\"');
  return `"${result}"`;
}

/**
 * Throw an exception if the tableName has invalid number of prefix components.
 *
 * @param tableName: Unescaped table name. E.g. 'Alb"um'
 * @returns tableName
 */
function validateTableName(tableName: TableName): TableName {
  if (tableName.length <= 2 && tableName.length > 0)
    return tableName;
  else
    throw new Error(`${tableName.join(".")} is not a valid table`);
}

/**
 * @param ts
 * @returns last section of a qualified table array. E.g. [a,b] -> [b]
 */
export function getTableNameSansSchema(ts: Array<string>): Array<string> {
  return [ts[ts.length-1]];
}

/**
 *
 * @param tableName: Unescaped table name. E.g. 'Alb"um'
 * @returns Escaped table name. E.g. '"Alb\"um"'
 */
export function escapeTableName(tableName: TableName): string {
  return validateTableName(tableName).map(escapeIdentifier).join(".");
}

/**
 * @param tableName
 * @returns escaped tableName string with schema qualification removed
 *
 * This is useful in where clauses in returning statements where a qualified table name is invalid SQLite SQL.
 */
export function escapeTableNameSansSchema(tableName: TableName): string {
  return escapeTableName(getTableNameSansSchema(tableName));
}

export function json_object(relationships: TableRelationships[], fields: Fields, table: TableName, tableAlias: string): string {
  const result = Object.entries(fields).map(([fieldName, field]) => {
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
        return `'${fieldName}', ${relationship(relationships, rel, field, tableAlias)}`;
      case "object":
        throw new Error('Unsupported field type "object"');
      case "array":
        throw new Error('Unsupported field type "array"');
      default:
        return unreachable(field["type"]);
    }
  }).join(", ");

  return tag('json_object', `JSON_OBJECT(${result})`);
}

export function where_clause(relationships: TableRelationships[], expression: Expression, queryTableName: TableName, queryTableAlias: string): string {
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
        const bopRhs = generateComparisonValueFragment(expression.value, queryTableAlias, currentTableAlias);
        if(expression.operator == '_in_year') {
          return `cast(strftime('%Y', ${bopLhs}) as integer) = ${bopRhs}`;
        } else if(expression.operator == '_modulus_is_zero') {
          return `cast(${bopLhs} as integer) % ${bopRhs} = 0`;
        } else if(expression.operator == '_nand') {
          return `NOT (${bopLhs} AND ${bopRhs})`;
        } else if(expression.operator == '_nor') {
          return `NOT (${bopLhs} OR ${bopRhs})`;
        } else if(expression.operator == '_xor') {
          return `(${bopLhs} AND (NOT ${bopRhs})) OR ((NOT${bopRhs}) AND ${bopRhs})`;
        } else {
          const bop = bop_op(expression.operator);
          return `${bopLhs} ${bop} ${bopRhs}`;
        }

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

function calculateExistsJoinInfo(allTableRelationships: TableRelationships[], exists: ExistsExpression, sourceTableName: TableName, sourceTableAlias: string): ExistsJoinInfo {
  switch (exists.in_table.type) {
    case "related":
      const tableRelationships = find_table_relationship(allTableRelationships, sourceTableName);
      const relationship = tableRelationships.relationships[exists.in_table.relationship];
      const joinTableAlias = generateTableAlias(relationship.target_table);

      const joinComparisonFragments = generateRelationshipJoinComparisonFragments(relationship, sourceTableAlias, joinTableAlias);

      return {
        joinTableName: relationship.target_table,
        joinTableAlias,
        joinComparisonFragments,
      };

    case "unrelated":
      return {
        joinTableName: exists.in_table.table,
        joinTableAlias: generateTableAlias(exists.in_table.table),
        joinComparisonFragments: []
      };

    default:
      return unreachable(exists.in_table["type"]);
  }
}

function generateRelationshipJoinComparisonFragments(relationship: Relationship, sourceTableAlias: string, targetTableAlias: string): string[] {
  const sourceTablePrefix = `${sourceTableAlias}.`;
  return Object
    .entries(relationship.column_mapping)
    .map(([sourceColumnName, targetColumnName]) =>
      `${sourceTablePrefix}${escapeIdentifier(sourceColumnName)} = ${targetTableAlias}.${escapeIdentifier(targetColumnName)}`);
}

function generateComparisonColumnFragment(comparisonColumn: ComparisonColumn, queryTableAlias: string, currentTableAlias: string): string {
  const path = comparisonColumn.path ?? [];
  const queryTablePrefix = queryTableAlias ? `${queryTableAlias}.` : '';
  const currentTablePrefix = currentTableAlias ? `${currentTableAlias}.` : '';
  if (path.length === 0) {
    return `${currentTablePrefix}${escapeIdentifier(comparisonColumn.name)}`
  } else if (path.length === 1 && path[0] === "$") {
    return `${queryTablePrefix}${escapeIdentifier(comparisonColumn.name)}`
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

function generateTableAlias(tableName: TableName): string {
  return generateIdentifierAlias(validateTableName(tableName).join("_"))
}

function generateIdentifierAlias(identifier: string): string {
  const randomSuffix = nanoid();
  return escapeIdentifier(`${identifier}_${randomSuffix}`);
}

/**
 *
 * @param allTableRelationships Array of Table Relationships
 * @param tableName Table Name
 * @returns Relationships matching table-name
 */
function find_table_relationship(allTableRelationships: TableRelationships[], tableName: TableName): TableRelationships {
  for(var i = 0; i < allTableRelationships.length; i++) {
    const r = allTableRelationships[i];
    if(tableNameEquals(r.source_table)(tableName)) {
      return r;
    }
  }
  throw new Error(`Couldn't find table relationships for table ${tableName} - This shouldn't happen.`);
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
 */
function aggregates_query(
    ts: TableRelationships[],
    tableName: TableName,
    joinInfo: RelationshipJoinInfo | null,
    aggregates: Aggregates,
    wWhere: Expression | null,
    wLimit: number | null,
    wOffset: number | null,
    wOrder: OrderBy | null,
  ): string {
  const tableAlias = generateTableAlias(tableName);

  const orderByInfo = orderBy(ts, wOrder, tableName, tableAlias);
  const orderByJoinClauses = orderByInfo?.joinClauses.join(" ") ?? "";
  const orderByClause = orderByInfo?.orderByClause ?? "";

  const whereClause = where(ts, wWhere, joinInfo, tableName, tableAlias);
  const sourceSubquery = `SELECT ${tableAlias}.* FROM ${escapeTableName(tableName)} AS ${tableAlias} ${orderByJoinClauses} ${whereClause} ${orderByClause} ${limit(wLimit)} ${offset(wOffset)}`

  const aggregate_pairs = Object.entries(aggregates).map(([k,v]) => {
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

  return `'aggregates', (SELECT JSON_OBJECT(${aggregate_pairs}) FROM (${sourceSubquery}))`;
}

type RelationshipJoinInfo = {
  sourceTableAlias: string
  columnMapping: Record<string, string> // Mapping from source table column name to target table column name
}

function table_query(
    ts: TableRelationships[],
    tableName: TableName,
    joinInfo: RelationshipJoinInfo | null,
    fields: Fields | null,
    aggregates: Aggregates | null,
    wWhere: Expression | null,
    aggregatesLimit: number | null,
    wLimit: number | null,
    wOffset: number | null,
    wOrder: OrderBy | null,
  ): string {
  const tableAlias      = generateTableAlias(tableName);
  const aggregateSelect = aggregates ? [aggregates_query(ts, tableName, joinInfo, aggregates, wWhere, aggregatesLimit, wOffset, wOrder)] : [];
  // The use of the JSON function inside JSON_GROUP_ARRAY is necessary from SQLite 3.39.0 due to breaking changes in
  // SQLite. See https://sqlite.org/forum/forumpost/e3b101fb3234272b for more details. This approach still works fine
  // for older versions too.
  const fieldSelect     = fields === null ? [] : [`'rows', JSON_GROUP_ARRAY(JSON(j))`];
  const fieldFrom       = fields === null ? '' : (() => {
    const whereClause = where(ts, wWhere, joinInfo, tableName, tableAlias);
    // NOTE: The reuse of the 'j' identifier should be safe due to scoping. This is confirmed in testing.
    if(wOrder === null || wOrder.elements.length < 1) {
      return `FROM ( SELECT ${json_object(ts, fields, tableName, tableAlias)} AS j FROM ${escapeTableName(tableName)} AS ${tableAlias} ${whereClause} ${limit(wLimit)} ${offset(wOffset)})`;
    } else {
      const orderByInfo = orderBy(ts, wOrder, tableName, tableAlias);
      const orderByJoinClauses = orderByInfo?.joinClauses.join(" ") ?? "";
      const orderByClause = orderByInfo?.orderByClause ?? "";

      const innerSelect = `SELECT ${tableAlias}.* FROM ${escapeTableName(tableName)} AS ${tableAlias} ${orderByJoinClauses} ${whereClause} ${orderByClause} ${limit(wLimit)} ${offset(wOffset)}`;

      const wrappedQueryTableAlias = generateTableAlias(tableName);
      return `FROM (SELECT ${json_object(ts, fields, tableName, wrappedQueryTableAlias)} AS j FROM (${innerSelect}) AS ${wrappedQueryTableAlias})`;
    }
  })()

  return tag('table_query',`(SELECT JSON_OBJECT(${[...fieldSelect, ...aggregateSelect].join(', ')}) ${fieldFrom})`);
}

function relationship(ts: TableRelationships[], r: Relationship, field: RelationshipField, sourceTableAlias: string): string {
  const relationshipJoinInfo = {
    sourceTableAlias,
    targetTable: r.target_table,
    columnMapping: r.column_mapping,
  };

  // We force a limit of 1 for object relationships in case the user has configured a manual
  // "object" relationship that accidentally actually is an array relationship
  const [limit, aggregatesLimit] =
    r.relationship_type === "object"
      ? [1, 1]
      : [coerceUndefinedToNull(field.query.limit), coerceUndefinedToNull(field.query.aggregates_limit)];

  return tag("relationship", table_query(
    ts,
    r.target_table,
    relationshipJoinInfo,
    coerceUndefinedToNull(field.query.fields),
    coerceUndefinedToNull(field.query.aggregates),
    coerceUndefinedToNull(field.query.where),
    aggregatesLimit,
    limit,
    coerceUndefinedToNull(field.query.offset),
    coerceUndefinedToNull(field.query.order_by),
  ));
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
    // TODO: Check for coverage of these operators
    case 'equal':                 result = '='; break;
    case 'greater_than':          result = '>'; break;
    case 'greater_than_or_equal': result = '>='; break;
    case 'less_than':             result = '<'; break;
    case 'less_than_or_equal':    result = '<='; break;
    case '_like':                 result = 'LIKE'; break;
    case '_glob':                 result = 'GLOB'; break;
    case '_regexp':               result = 'REGEXP'; break; // TODO: Have capabilities detect if REGEXP support is enabled
    case '_and':                  result = 'AND'; break;
    case '_or':                   result = 'OR'; break;
  }
  // TODO: We can't always assume that we can include the operator here verbatim.
  return tag('bop_op',result);
}

function uop_op(o: UnaryComparisonOperator): string {
  let result = o;
  switch(o) {
    case 'is_null': result = "IS NULL"; break;
  }
  return tag('uop_op',result);
}

function orderDirection(orderDirection: OrderDirection): string {
  switch (orderDirection) {
    case "asc": return "ASC NULLS LAST";
    case "desc": return "DESC NULLS FIRST";
    default:
      return unreachable(orderDirection);
  }
}

type OrderByInfo = {
  joinClauses: string[],
  orderByClause: string,
}

function orderBy(allTableRelationships: TableRelationships[], orderBy: OrderBy | null, queryTableName: TableName, queryTableAlias: string): OrderByInfo | null {
  if (orderBy === null || orderBy.elements.length < 1) {
    return null;
  }

  const joinInfos = Object
    .entries(orderBy.relations)
    .flatMap(([subrelationshipName, subrelation]) =>
      generateOrderByJoinClause(allTableRelationships, orderBy.elements, [], subrelationshipName, subrelation, queryTableName, queryTableAlias)
    );

  const orderByFragments =
    orderBy.elements
      .map(orderByElement => {
        const targetTableAlias = orderByElement.target_path.length === 0
          ? queryTableAlias
          : (() => {
              const joinInfo = joinInfos.find(joinInfo => joinInfo.joinTableType === getJoinTableTypeForTarget(orderByElement.target) && stringArrayEquals(joinInfo.relationshipPath)(orderByElement.target_path));
              if (joinInfo === undefined) throw new Error("Can't find a join table for order by target."); // Should not happen 😉
              return joinInfo.tableAlias;
            })();

        const targetColumn = `${targetTableAlias}.${getOrderByTargetAlias(orderByElement.target)}`

        const targetExpression = orderByElement.target.type === "star_count_aggregate"
          ? `COALESCE(${targetColumn}, 0)`
          : targetColumn

        return `${targetExpression} ${orderDirection(orderByElement.order_direction)}`;
      });

  return {
    joinClauses: joinInfos.map(joinInfo => joinInfo.joinClause),
    orderByClause: tag('orderBy',`ORDER BY ${orderByFragments.join(",")}`),
  };
}

type OrderByJoinTableType = "column" | "aggregate";

function getJoinTableTypeForTarget(orderByTarget: OrderByTarget): OrderByJoinTableType {
  switch (orderByTarget.type) {
    case "column": return "column";
    case "star_count_aggregate": return "aggregate";
    case "single_column_aggregate": return "aggregate";
    default:
      return unreachable(orderByTarget["type"]);
  }
}

type OrderByJoinInfo = {
  joinTableType: OrderByJoinTableType,
  relationshipPath: string[],
  tableAlias: string,
  joinClause: string,
}

function generateOrderByJoinClause(
    allTableRelationships: TableRelationships[],
    allOrderByElements: OrderByElement[],
    parentRelationshipNames: string[],
    relationshipName: string,
    orderByRelation: OrderByRelation,
    sourceTableName: TableName,
    sourceTableAlias: string
  ): OrderByJoinInfo[] {
  const relationshipPath = [...parentRelationshipNames, relationshipName];
  const tableRelationships = find_table_relationship(allTableRelationships, sourceTableName);
  const relationship = tableRelationships.relationships[relationshipName];

  const orderByElements = allOrderByElements.filter(byTargetPath(relationshipPath));
  const columnTargetsExist = orderByElements.some(element => getJoinTableTypeForTarget(element.target) === "column");
  const aggregateElements = orderByElements.filter(element => getJoinTableTypeForTarget(element.target) === "aggregate");

  const [columnTargetJoin, subrelationJoinInfo] = (() => {
    const subrelationsExist = Object.keys(orderByRelation.subrelations).length > 0;
    if (columnTargetsExist || subrelationsExist) {
      const columnTargetJoin = generateOrderByColumnTargetJoinInfo(allTableRelationships, relationshipPath, relationship, sourceTableAlias, orderByRelation.where);

      const subrelationJoinInfo = Object
        .entries(orderByRelation.subrelations)
        .flatMap(([subrelationshipName, subrelation]) =>
          generateOrderByJoinClause(allTableRelationships, allOrderByElements, relationshipPath, subrelationshipName, subrelation, relationship.target_table, columnTargetJoin.tableAlias)
        );

      return [[columnTargetJoin], subrelationJoinInfo]

    } else {
      return [[], []];
    }
  })();

  const aggregateTargetJoin = aggregateElements.length > 0
    ? [generateOrderByAggregateTargetJoinInfo(allTableRelationships, relationshipPath, relationship, sourceTableAlias, orderByRelation.where, aggregateElements)]
    : [];


  return [
    ...columnTargetJoin,
    ...aggregateTargetJoin,
    ...subrelationJoinInfo
  ];
}

const byTargetPath = (relationshipPath: string[]) => (orderByElement: OrderByElement): boolean => stringArrayEquals(orderByElement.target_path)(relationshipPath);

function generateOrderByColumnTargetJoinInfo(
    allTableRelationships: TableRelationships[],
    relationshipPath: string[],
    relationship: Relationship,
    sourceTableAlias: string,
    whereExpression: Expression | undefined
  ): OrderByJoinInfo {
  const targetTableAlias = generateTableAlias(relationship.target_table);

  const joinComparisonFragments = generateRelationshipJoinComparisonFragments(relationship, sourceTableAlias, targetTableAlias);
  const whereComparisons = whereExpression ? [where_clause(allTableRelationships, whereExpression, relationship.target_table, targetTableAlias)] : [];
  const joinOnFragment = [...joinComparisonFragments, ...whereComparisons].join(" AND ");

  const joinClause = tag("columnTargetJoin", `LEFT JOIN ${escapeTableName(relationship.target_table)} AS ${targetTableAlias} ON ${joinOnFragment}`);
  return {
    joinTableType: "column",
    relationshipPath: relationshipPath,
    tableAlias: targetTableAlias,
    joinClause: joinClause
  };
}

function generateOrderByAggregateTargetJoinInfo(
  allTableRelationships: TableRelationships[],
  relationshipPath: string[],
  relationship: Relationship,
  sourceTableAlias: string,
  whereExpression: Expression | undefined,
  aggregateElements: OrderByElement[],
): OrderByJoinInfo {
  const targetTableAlias = generateTableAlias(relationship.target_table);
  const subqueryTableAlias = generateTableAlias(relationship.target_table);

  const aggregateColumnsFragments = aggregateElements.flatMap(element => {
    switch (element.target.type) {
      case "column": return [];
      case "star_count_aggregate": return `COUNT(*) AS ${getOrderByTargetAlias(element.target)}`;
      case "single_column_aggregate": return `${cast_aggregate_function(element.target.function)}(${escapeIdentifier(element.target.column)}) AS ${getOrderByTargetAlias(element.target)}`;
      default: unreachable(element.target["type"]);
    }
  });
  const joinColumns = Object.values(relationship.column_mapping).map(escapeIdentifier);
  const selectColumns = [...joinColumns, aggregateColumnsFragments];
  const whereClause = whereExpression ? `WHERE ${where_clause(allTableRelationships, whereExpression, relationship.target_table, subqueryTableAlias)}` : "";
  const aggregateSubquery = `SELECT ${selectColumns.join(", ")} FROM ${escapeTableName(relationship.target_table)} AS ${subqueryTableAlias} ${whereClause} GROUP BY ${joinColumns.join(", ")}`

  const joinComparisonFragments = generateRelationshipJoinComparisonFragments(relationship, sourceTableAlias, targetTableAlias);
  const joinOnFragment = [ ...joinComparisonFragments ].join(" AND ");
  const joinClause = tag("aggregateTargetJoin", `LEFT JOIN (${aggregateSubquery}) AS ${targetTableAlias} ON ${joinOnFragment}`)
  return {
    joinTableType: "aggregate",
    relationshipPath: relationshipPath,
    tableAlias: targetTableAlias,
    joinClause: joinClause
  };
}

function getOrderByTargetAlias(orderByTarget: OrderByTarget): string {
  switch (orderByTarget.type) {
    case "column": return escapeIdentifier(orderByTarget.column);
    case "star_count_aggregate": return escapeIdentifier("__star_count__");
    case "single_column_aggregate": return escapeIdentifier(`__${orderByTarget.function}_${orderByTarget.column}__`);
    default:
      return unreachable(orderByTarget["type"]);
  }
}

/**
 * @param whereExpression Nested expression used in the associated where clause
 * @param joinInfo Information about a possible join from a source table to the query table that needs to be generated into the where clause
 * @returns string representing the combined where clause
 */
function where(ts: TableRelationships[], whereExpression: Expression | null, joinInfo: RelationshipJoinInfo | null, queryTableName: TableName, queryTableAlias: string): string {
  const whereClause = whereExpression !== null ? [where_clause(ts, whereExpression, queryTableName, queryTableAlias)] : [];
  const joinArray = joinInfo
    ? Object
      .entries(joinInfo.columnMapping)
      .map(([sourceColumn, targetColumn]) =>
        `${joinInfo.sourceTableAlias}.${escapeIdentifier(sourceColumn)} = ${queryTableAlias}.${escapeIdentifier(targetColumn)}`
      )
    : []

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

function query(request: TableRequest): string {
  const tableRelationships = only_table_relationships(request.table_relationships);
  const result = table_query(
    tableRelationships,
    request.table,
    null,
    coerceUndefinedToNull(request.query.fields),
    coerceUndefinedToNull(request.query.aggregates),
    coerceUndefinedToNull(request.query.where),
    coerceUndefinedToNull(request.query.aggregates_limit),
    coerceUndefinedToNull(request.query.limit),
    coerceUndefinedToNull(request.query.offset),
    coerceUndefinedToNull(request.query.order_by),
    );
  return tag('query', `SELECT ${result} as data`);
}

/**
 * Creates a SELECT statement that returns rows for the foreach ids.
 *
 * Given:
 * ```
 * [
 *   {"columnA": {"value": "A1", "value_type": "string" }, "columnB": {"value": B1, "value_type": "string" }},
 *   {"columnA": {"value": "A2", "value_type": "string" }, "columnB": {"value": B2, "value_type": "string" }}
 * ]
 * ```
 *
 * We will generate the following SQL:
 *
 * ```
 * SELECT value ->> '$.columnA' AS "columnA", value ->> '$.columnB' AS "columnB"
 * FROM JSON_EACH('[{"columnA":"A1","columnB":"B1"},{"columnA":"A2","columnB":"B2"}]')
 * ```
 */
function foreach_ids_table_value(foreachIds: Record<string, ScalarValue>[]): string {
  const columnNames = Object.keys(foreachIds[0]);

  const columns = columnNames.map(name => `value ->> ${escapeString("$." + name)} AS ${escapeIdentifier(name)}`);
  const jsonData = foreachIds.map(ids => mapObject(ids, ([column, scalarValue]) => [column, scalarValue.value]));

  return tag('foreach_ids_table_value', `SELECT ${columns} FROM JSON_EACH(${escapeString(JSON.stringify(jsonData))})`)
}

/**
 * Creates SQL query for a foreach query request.
 *
 * This is done by creating a CTE table that contains the foreach ids, and then wrapping
 * the existing query in a new one that joins from the CTE virtual table to the original query table
 * using a generated table relationship and fields list.
 *
 * The SQL we generate looks like this:
 *
 *```
 * WITH foreach_ids_xxx AS (
 *   SELECT ... FROM ... (see foreach_ids_table_value)
 * )
 * SELECT table_subquery AS data
 * ```
 */
function foreach_query(foreachIds: Record<string, ScalarValue>[], request: TableRequest): string {
  const randomSuffix = nanoid();
  const foreachTableName: TableName = [`foreach_ids_${randomSuffix}`];
  const foreachRelationshipName = "Foreach";
  const foreachTableRelationship: TableRelationships = {
    type: 'table',
    source_table: foreachTableName,
    relationships: {
      [foreachRelationshipName]: {
        relationship_type: "array",
        target_table: request.table,
        column_mapping: mapObject(foreachIds[0], ([columnName, _scalarValue]) => [columnName, columnName])
      }
    }
  };
  const foreachQueryFields: Record<string, Field> = {
    "query": {
      type: "relationship",
      relationship: foreachRelationshipName,
      query: request.query
    }
  };

  const foreachIdsTableValue = foreach_ids_table_value(foreachIds);
  const tableRelationships = only_table_relationships(request.table_relationships);
  const tableSubquery = table_query(
    [foreachTableRelationship, ...(tableRelationships)],
    foreachTableName,
    null,
    foreachQueryFields,
    null,
    null,
    null,
    null,
    null,
    null,
    );
  return tag('foreach_query', `WITH ${escapeTableName(foreachTableName)} AS (${foreachIdsTableValue}) SELECT ${tableSubquery} AS data`);
}

function only_table_relationships(all: Array<TableRelationships | FunctionRelationships>): Array<TableRelationships> {
  return all.filter(isTableRelationship);
}

function isTableRelationship(relationships: TableRelationships | FunctionRelationships,): relationships is TableRelationships {
  return (relationships as TableRelationships).source_table !== undefined;
}

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
export async function queryData(config: Config, sqlLogger: SqlLogger, request: TableRequest): Promise<QueryResponse> {
  return await withConnection(config, defaultMode, sqlLogger, async db => {
    const q =
    request.foreach
        ? foreach_query(request.foreach, request)
        : query(request);

    if(q.length > QUERY_LENGTH_LIMIT) {
      const error = new ErrorWithStatusCode(
        `Generated SQL Query was too long (${q.length} > ${QUERY_LENGTH_LIMIT})`,
        500,
        { "query.length": q.length, "limit": QUERY_LENGTH_LIMIT }
      );
      throw error;
    }

    const results = await db.query(q);
    return JSON.parse(results[0].data);
  });
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
export async function explain(config: Config, sqlLogger: SqlLogger, request: TableRequest): Promise<ExplainResponse> {
  return await withConnection(config, defaultMode, sqlLogger, async db => {
    const q = query(request);
    const result = await db.query(`EXPLAIN QUERY PLAN ${q}`);
    return {
      query: q,
      lines: [ "", ...formatExplainLines(result as AnalysisEntry[])]
    };
  });
}

function formatExplainLines(items: AnalysisEntry[]): string[] {
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

