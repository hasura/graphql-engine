import { Config }  from "./config";
import { defaultMode, SqlLogger, withConnection } from "./db";
import { coerceUndefinedToNull, tableNameEquals, unreachable, stringArrayEquals, ErrorWithStatusCode, mapObject } from "./util";
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
    InterpolatedRelationships,
    Target,
    InterpolatedQueries,
    Relationships,
    InterpolatedQuery,
    InterpolatedItem,
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

export function escapeTargetName(target: Target): string {
  switch(target.type) {
    case 'table':
      return escapeTableName(target.name);
    case 'interpolated':
      return escapeTableName([target.id]); // Interpret as CTE reference
    default:
      throw(new ErrorWithStatusCode('`escapeTargetName` only implemented for tables and interpolated queries', 500, {target}));
  }
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

export function json_object(all_relationships: Relationships[], fields: Fields, target: Target, tableAlias: string): string {
  const result = Object.entries(fields).map(([fieldName, field]) => {
    switch(field.type) {
      case "column":
        return `${escapeString(fieldName)}, ${escapeIdentifier(field.column)}`;
      case "relationship":
        const relationships = find_relationships(all_relationships, target);
        const rel = relationships.relationships[field.relationship];
        if(rel === undefined) {
          throw new Error(`Couldn't find relationship ${field.relationship} for field ${fieldName} on target ${JSON.stringify(target)}`);
        }
        return `'${fieldName}', ${relationship(all_relationships, rel, field, tableAlias)}`;
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

export function where_clause(relationships: Relationships[], expression: Expression, queryTarget: Target, queryTableAlias: string): string {
  const generateWhere = (expression: Expression, currentTarget: Target, currentTableAlias: string): string => {
    switch(expression.type) {
      case "not":
        const aNot = generateWhere(expression.expression, currentTarget, currentTableAlias);
          return `(NOT ${aNot})`;

      case "and":
        const aAnd = expression.expressions.flatMap(x => generateWhere(x, currentTarget, currentTableAlias));
        return aAnd.length > 0
          ? `(${aAnd.join(" AND ")})`
          : "(1 = 1)" // true

      case "or":
        const aOr = expression.expressions.flatMap(x => generateWhere(x, currentTarget, currentTableAlias));
        return aOr.length > 0
          ? `(${aOr.join(" OR ")})`
          : "(1 = 0)" // false

      case "exists":
        const joinInfo = calculateExistsJoinInfo(relationships, expression, currentTarget, currentTableAlias);
        const tableTarget: Target = joinInfo.joinTarget;
        const subqueryWhere = generateWhere(expression.where, tableTarget, joinInfo.joinTableAlias);
        const whereComparisons = [...joinInfo.joinComparisonFragments, subqueryWhere].join(" AND ");
        return tag('exists',`EXISTS (SELECT 1 FROM ${escapeTargetName(joinInfo.joinTarget)} AS ${joinInfo.joinTableAlias} WHERE ${whereComparisons})`);

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

  return generateWhere(expression, queryTarget, queryTableAlias);
}

type ExistsJoinInfo = {
  joinTarget: Target,
  joinTableAlias: string,
  joinComparisonFragments: string[]
}

function calculateExistsJoinInfo(allRelationships: Relationships[], exists: ExistsExpression, sourceTarget: Target, sourceTableAlias: string): ExistsJoinInfo {
  switch (exists.in_table.type) {
    case "related":
      const tableRelationships = find_relationships(allRelationships, sourceTarget);
      const relationship = tableRelationships.relationships[exists.in_table.relationship];
      const joinTableAlias = generateTargetAlias(relationship.target);

      const joinComparisonFragments = generateRelationshipJoinComparisonFragments(relationship, sourceTableAlias, joinTableAlias);

      return {
        joinTarget: relationship.target,
        joinTableAlias,
        joinComparisonFragments,
      };

    case "unrelated":
      return {
        joinTarget: {type: 'table', name: exists.in_table.table},
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
      `${sourceTablePrefix}${escapeIdentifier(sourceColumnName)} = ${targetTableAlias}.${escapeIdentifier(targetColumnName as string)}`);
}

function generateComparisonColumnFragment(comparisonColumn: ComparisonColumn, queryTableAlias: string, currentTableAlias: string): string {
  const path = comparisonColumn.path ?? [];
  const queryTablePrefix = queryTableAlias ? `${queryTableAlias}.` : '';
  const currentTablePrefix = currentTableAlias ? `${currentTableAlias}.` : '';
  const selector = getColumnSelector(comparisonColumn.name);
  if (path.length === 0) {
    return `${currentTablePrefix}${escapeIdentifier(selector)}`
  } else if (path.length === 1 && path[0] === "$") {
    return `${queryTablePrefix}${escapeIdentifier(selector)}`
  } else {
    throw new Error(`Unsupported path on ComparisonColumn: ${[...path, selector].join(".")}`);
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

export function generateTargetAlias(target: Target): string {
  switch(target.type) {
    case 'function':
      throw new ErrorWithStatusCode("Can't create alias for functions", 500, {target});
    case 'interpolated':
      return generateTableAlias([target.id]);
    case 'table':
      return generateTableAlias(target.name);
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
function find_relationships(allRelationships: Relationships[], target: Target): Relationships {
  switch(target.type) {
    case 'table':
      for(var i = 0; i < allRelationships.length; i++) {
        const r = allRelationships[i] as TableRelationships;
        if(r.source_table != undefined && tableNameEquals(r.source_table)(target)) {
          return r;
        }
      }
      break
    case 'interpolated':
      for(var i = 0; i < allRelationships.length; i++) {
        const r = allRelationships[i] as InterpolatedRelationships;
        if(r.source_interpolated_query != undefined && r.source_interpolated_query == target.id) {
          return r;
        }
      }
      break;
  }
  throw new Error(`Couldn't find table relationships for target ${JSON.stringify(target)} - This shouldn't happen.`);
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
    allRelationships: Relationships[],
    target: Target,
    joinInfo: RelationshipJoinInfo | null,
    aggregates: Aggregates,
    wWhere: Expression | null,
    wLimit: number | null,
    wOffset: number | null,
    wOrder: OrderBy | null,
  ): string {

  const tableAlias = generateTargetAlias(target);
  const orderByInfo = orderBy(allRelationships, wOrder, target, tableAlias);
  const orderByJoinClauses = orderByInfo?.joinClauses.join(" ") ?? "";
  const orderByClause = orderByInfo?.orderByClause ?? "";
  const whereClause = where(allRelationships, wWhere, joinInfo, target, tableAlias);
  const sourceSubquery = `SELECT ${tableAlias}.* FROM ${escapeTargetName(target)} AS ${tableAlias} ${orderByJoinClauses} ${whereClause} ${orderByClause} ${limit(wLimit)} ${offset(wOffset)}`

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

function target_query( // TODO: Rename as `target_query`
    allRelationships: Relationships[],
    target: Target,
    joinInfo: RelationshipJoinInfo | null,
    fields: Fields | null,
    aggregates: Aggregates | null,
    wWhere: Expression | null,
    aggregatesLimit: number | null,
    wLimit: number | null,
    wOffset: number | null,
    wOrder: OrderBy | null,
  ): string {
  
  var tableName;

  // TableNames are resolved from IDs when using NQs.
  switch(target.type) {
    case 'table':        tableName = target.name; break;
    case 'interpolated': tableName = [target.id]; break;
    case 'function':
      throw new ErrorWithStatusCode(`Can't execute table_query for UDFs`, 500, {target});
  }

  const tableAlias      = generateTargetAlias(target);
  const aggregateSelect = aggregates ? [aggregates_query(allRelationships, target, joinInfo, aggregates, wWhere, aggregatesLimit, wOffset, wOrder)] : [];
  // The use of the JSON function inside JSON_GROUP_ARRAY is necessary from SQLite 3.39.0 due to breaking changes in
  // SQLite. See https://sqlite.org/forum/forumpost/e3b101fb3234272b for more details. This approach still works fine
  // for older versions too.
  const fieldSelect     = fields === null ? [] : [`'rows', JSON_GROUP_ARRAY(JSON(j))`];
  const fieldFrom       = fields === null ? '' : (() => {
    const whereClause = where(allRelationships, wWhere, joinInfo, target, tableAlias);
    // NOTE: The reuse of the 'j' identifier should be safe due to scoping. This is confirmed in testing.
    if(wOrder === null || wOrder.elements.length < 1) {
      return `FROM ( SELECT ${json_object(allRelationships, fields, target, tableAlias)} AS j FROM ${escapeTableName(tableName)} AS ${tableAlias} ${whereClause} ${limit(wLimit)} ${offset(wOffset)})`;
    } else {
      const orderByInfo = orderBy(allRelationships, wOrder, target, tableAlias);
      const orderByJoinClauses = orderByInfo?.joinClauses.join(" ") ?? "";
      const orderByClause = orderByInfo?.orderByClause ?? "";

      const innerSelect = `SELECT ${tableAlias}.* FROM ${escapeTableName(tableName)} AS ${tableAlias} ${orderByJoinClauses} ${whereClause} ${orderByClause} ${limit(wLimit)} ${offset(wOffset)}`;

      const wrappedQueryTableAlias = generateTableAlias(tableName);
      return `FROM (SELECT ${json_object(allRelationships, fields, target, wrappedQueryTableAlias)} AS j FROM (${innerSelect}) AS ${wrappedQueryTableAlias})`;
    }
  })()

  return tag('table_query',`(SELECT JSON_OBJECT(${[...fieldSelect, ...aggregateSelect].join(', ')}) ${fieldFrom})`);
}

function relationship(ts: Relationships[], r: Relationship, field: RelationshipField, sourceTableAlias: string): string {
  const relationshipJoinInfo = {
    sourceTableAlias,
    columnMapping: r.column_mapping as Record<string, string>,
  };

  // We force a limit of 1 for object relationships in case the user has configured a manual
  // "object" relationship that accidentally actually is an array relationship
  const [limit, aggregatesLimit] =
    r.relationship_type === "object"
      ? [1, 1]
      : [coerceUndefinedToNull(field.query.limit), coerceUndefinedToNull(field.query.aggregates_limit)];

  return tag("relationship", target_query(
    ts,
    r.target,
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

function orderBy(allRelationships: Relationships[], orderBy: OrderBy | null, queryTarget: Target, queryTableAlias: string): OrderByInfo | null {
  if (orderBy === null || orderBy.elements.length < 1) {
    return null;
  }

  const joinInfos = Object
    .entries(orderBy.relations)
    .flatMap(([subrelationshipName, subrelation]) =>
      generateOrderByJoinClause(allRelationships, orderBy.elements, [], subrelationshipName, subrelation, queryTarget, queryTableAlias)
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
    allRelationships: Relationships[],
    allOrderByElements: OrderByElement[],
    parentRelationshipNames: string[],
    relationshipName: string,
    orderByRelation: OrderByRelation,
    sourceTarget: Target,
    sourceTableAlias: string
  ): OrderByJoinInfo[] {
  const relationshipPath = [...parentRelationshipNames, relationshipName];
  const relationships = find_relationships(allRelationships, sourceTarget);
  const relationship = relationships.relationships[relationshipName];

  const orderByElements = allOrderByElements.filter(byTargetPath(relationshipPath));
  const columnTargetsExist = orderByElements.some(element => getJoinTableTypeForTarget(element.target) === "column");
  const aggregateElements = orderByElements.filter(element => getJoinTableTypeForTarget(element.target) === "aggregate");

  const [columnTargetJoin, subrelationJoinInfo] = (() => {
    const subrelationsExist = Object.keys(orderByRelation.subrelations).length > 0;
    if (columnTargetsExist || subrelationsExist) {
      const columnTargetJoin = generateOrderByColumnTargetJoinInfo(allRelationships, relationshipPath, relationship, sourceTableAlias, orderByRelation.where);

      const subrelationJoinInfo = Object
        .entries(orderByRelation.subrelations)
        .flatMap(([subrelationshipName, subrelation]) =>
          generateOrderByJoinClause(allRelationships, allOrderByElements, relationshipPath, subrelationshipName, subrelation, relationship.target, columnTargetJoin.tableAlias)
        );

      return [[columnTargetJoin], subrelationJoinInfo]

    } else {
      return [[], []];
    }
  })();

  const aggregateTargetJoin = aggregateElements.length > 0
    ? [generateOrderByAggregateTargetJoinInfo(allRelationships, relationshipPath, relationship, sourceTableAlias, orderByRelation.where, aggregateElements)]
    : [];


  return [
    ...columnTargetJoin,
    ...aggregateTargetJoin,
    ...subrelationJoinInfo
  ];
}

const byTargetPath = (relationshipPath: string[]) => (orderByElement: OrderByElement): boolean => stringArrayEquals(orderByElement.target_path)(relationshipPath);

function generateOrderByColumnTargetJoinInfo(
    allRelationships: Relationships[],
    relationshipPath: string[],
    relationship: Relationship,
    sourceTableAlias: string,
    whereExpression: Expression | undefined
  ): OrderByJoinInfo {
  const targetTableAlias = generateTargetAlias(relationship.target);

  const joinComparisonFragments = generateRelationshipJoinComparisonFragments(relationship, sourceTableAlias, targetTableAlias);
  const whereComparisons = whereExpression ? [where_clause(allRelationships, whereExpression, relationship.target, targetTableAlias)] : [];
  const joinOnFragment = [...joinComparisonFragments, ...whereComparisons].join(" AND ");

  const joinClause = tag("columnTargetJoin", `LEFT JOIN ${escapeTargetName(relationship.target)} AS ${targetTableAlias} ON ${joinOnFragment}`);
  return {
    joinTableType: "column",
    relationshipPath: relationshipPath,
    tableAlias: targetTableAlias,
    joinClause: joinClause
  };
}

function generateOrderByAggregateTargetJoinInfo(
  allTableRelationships: Relationships[],
  relationshipPath: string[],
  relationship: Relationship,
  sourceTableAlias: string,
  whereExpression: Expression | undefined,
  aggregateElements: OrderByElement[],
): OrderByJoinInfo {

  const targetTableAlias = generateTargetAlias(relationship.target);
  const subqueryTableAlias = generateTargetAlias(relationship.target);

  const aggregateColumnsFragments = aggregateElements.flatMap(element => {
    switch (element.target.type) {
      case "column": return [];
      case "star_count_aggregate": return `COUNT(*) AS ${getOrderByTargetAlias(element.target)}`;
      case "single_column_aggregate": return `${cast_aggregate_function(element.target.function)}(${escapeIdentifier(element.target.column)}) AS ${getOrderByTargetAlias(element.target)}`;
      default: unreachable(element.target["type"]);
    }
  });
  const joinColumns = Object.values(relationship.column_mapping as Record<string, string>).map(escapeIdentifier);
  const selectColumns = [...joinColumns, aggregateColumnsFragments];
  const whereClause = whereExpression ? `WHERE ${where_clause(allTableRelationships, whereExpression, relationship.target, subqueryTableAlias)}` : "";
  const aggregateSubquery = `SELECT ${selectColumns.join(", ")} FROM ${escapeTargetName(relationship.target)} AS ${subqueryTableAlias} ${whereClause} GROUP BY ${joinColumns.join(", ")}`

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
    case "column": return escapeIdentifier(getColumnSelector(orderByTarget.column));
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
function where(allRelationships: Relationships[], whereExpression: Expression | null, joinInfo: RelationshipJoinInfo | null, queryTarget: Target, queryTableAlias: string): string {
  const whereClause = whereExpression !== null ? [where_clause(allRelationships, whereExpression, queryTarget, queryTableAlias)] : [];
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

// NOTE: InterpolatedItem can be extended to support arrays of values.
// NOTE: It appears that value.value_type comes back as `Boolean` when we advertise only items from `ScalarTypeKey`
function cte_item(value: InterpolatedItem): string {
  switch(value.type) {
    case 'text':
      return value.value;
    case 'scalar':
      switch(value.value_type.toLowerCase()) {
        // Check this list against the types listed in capabilities
        case 'string':
          return escapeString(value.value);
        case 'number':
        case 'int':
        case 'integer':
        case 'float':
          return `${value.value}`;
        case 'bool':
        case 'boolean':
          return `${value.value ? 1 : 0}`;
        // Assume that everything else is a JSON value
        case 'json':
        default:
          return `json( ${ escapeString(JSON.stringify(value.value)) } )`;
      }
    default:
      return unreachable(value["type"]);
  }
}

function cte_items(iq: InterpolatedQuery): string {
  const items = iq.items.map(cte_item);
  const joined = items.join(' ');
  return `${iq.id} AS ( ${joined} )`;
}

function cte_block(qs: InterpolatedQueries): string {
  const ctes = Object.entries(qs).map(([_id, iq], _ix) => cte_items(iq));
  const cte_string = ctes.join(', ');
  return `WITH ${cte_string}`;
}

function query(request: QueryRequest): string {
  const cte = request.interpolated_queries == null ? '' : cte_block(request.interpolated_queries);
  const result = target_query(
    request.relationships,
    request.target,
    null,
    coerceUndefinedToNull(request.query.fields),
    coerceUndefinedToNull(request.query.aggregates),
    coerceUndefinedToNull(request.query.where),
    coerceUndefinedToNull(request.query.aggregates_limit),
    coerceUndefinedToNull(request.query.limit),
    coerceUndefinedToNull(request.query.offset),
    coerceUndefinedToNull(request.query.order_by),
    );
  return tag('query', `${cte} SELECT ${result} as data`);
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
function foreach_query(foreachIds: Record<string, ScalarValue>[], request: QueryRequest): string {

  const randomSuffix = nanoid();
  const foreachTableName: TableName = [`foreach_ids_${randomSuffix}`];
  const foreachRelationshipName = "Foreach";
  const foreachTableRelationship: TableRelationships = {
    type: 'table',
    source_table: foreachTableName,
    relationships: {
      [foreachRelationshipName]: {
        relationship_type: "array",
        target: request.target,
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
  const tableSubquery = target_query(
    [foreachTableRelationship, ...(request.relationships)],
    {type: 'table', name: foreachTableName}, // Note: expand to other target types
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
export async function queryData(config: Config, sqlLogger: SqlLogger, request: QueryRequest): Promise<QueryResponse> {
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
export async function explain(config: Config, sqlLogger: SqlLogger, request: QueryRequest): Promise<ExplainResponse> {
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

const getColumnSelector = (columnSelector: string | Array<string>): string => {
  if (typeof columnSelector === "string")
    return columnSelector;
  return columnSelector[0];
}
