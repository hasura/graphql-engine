import { QueryRequest, TableRelationships, Relationship, Query, Field, OrderBy, Expression, BinaryComparisonOperator, UnaryComparisonOperator, BinaryArrayComparisonOperator, ComparisonColumn, ComparisonValue, Aggregate, SingleColumnAggregate, ColumnCountAggregate, TableName, OrderByElement, OrderByRelation, ExistsInTable, ExistsExpression } from "@hasura/dc-api-types";
import { coerceUndefinedToNull, filterIterable, mapIterable, reduceAndIterable, reduceOrIterable, skipIterable, tableNameEquals, takeIterable, unreachable } from "./util";
import * as math from "mathjs";

type RelationshipName = string

type ScalarValue = (string | number | boolean | null)

// This is a more constrained type for response rows that knows that the reference
// agent never returns custom scalars that are JSON objects
type ProjectedRow = {
  [fieldName: string]: ScalarValue | QueryResponse
}

// We need a more constrained version of QueryResponse that uses ProjectedRow for rows
type QueryResponse = {
  aggregates?: Record<string, ScalarValue> | null,
  rows?: ProjectedRow[] | null
}

const prettyPrintBinaryComparisonOperator = (operator: BinaryComparisonOperator): string => {
  switch (operator) {
    case "greater_than": return ">";
    case "greater_than_or_equal": return ">=";
    case "less_than": return "<";
    case "less_than_or_equal": return "<=";
    case "equal": return "==";
    case "same_day_as": return "same_day_as";
    case "in_year": return "in_year";
    default: return unknownOperator(operator);
  };
};

const prettyPrintBinaryArrayComparisonOperator = (operator: BinaryArrayComparisonOperator): string => {
  switch (operator) {
    case "in": return "IN";
    default: return unknownOperator(operator);
  };
};

const prettyPrintUnaryComparisonOperator = (operator: UnaryComparisonOperator): string => {
  switch (operator) {
    case "is_null": return "IS NULL";
    default: return unknownOperator(operator);
  };
};

const dateTimeSameDayAs = (a: ScalarValue, b: ScalarValue): boolean => {
  if (typeof a !== "string")
    return expectedString(typeof a);

  if (typeof b !== "string")
    return expectedString(typeof b);

  return a.substring(0, 10) === b.substring(0, 10);
}

const dateTimeInYear = (a: ScalarValue, b: ScalarValue): boolean => {
  if (typeof a !== "string")
    return expectedString(typeof a);

  if (typeof b !== "number")
    return expectedNumber(typeof b);

  const bString = b.toString();
  return bString.length === 4 && a.startsWith(bString);
}

const getBinaryComparisonOperatorEvaluator = (operator: BinaryComparisonOperator): ((left: ScalarValue, right: ScalarValue) => boolean) => {
  switch (operator) {
    case "greater_than": return (a, b) => a !== null && b !== null && a > b;
    case "greater_than_or_equal": return (a, b) => a !== null && b !== null && a >= b;
    case "less_than": return (a, b) => a !== null && b !== null && a < b;
    case "less_than_or_equal": return (a, b) => a !== null && b !== null && a <= b;
    case "equal": return (a, b) => a !== null && b !== null && a === b;
    case "same_day_as": return dateTimeSameDayAs;
    case "in_year": return dateTimeInYear;
    default: return unknownOperator(operator);
  };
};

const getBinaryArrayComparisonOperatorEvaluator = (operator: BinaryArrayComparisonOperator): ((left: ScalarValue, right: ScalarValue[]) => boolean) => {
  switch (operator) {
    case "in": return (a, bs) => a !== null && bs.includes(a);
    default: return unknownOperator(operator);
  };
};


const getUnaryComparisonOperatorEvaluator = (operator: UnaryComparisonOperator): ((value: ScalarValue) => boolean) => {
  switch (operator) {
    case "is_null": return (v) => v === null;
    default: return unknownOperator(operator);
  };
};

const prettyPrintComparisonColumn = (comparisonColumn: ComparisonColumn): string => {
  return (comparisonColumn.path ?? []).concat(comparisonColumn.name).map(p => `[${p}]`).join(".");
}

const prettyPrintComparisonValue = (comparisonValue: ComparisonValue): string => {
  switch (comparisonValue.type) {
    case "column":
      return prettyPrintComparisonColumn(comparisonValue.column);
    case "scalar":
      return comparisonValue.value === null ? "null" : comparisonValue.value.toString();
    default:
      return unreachable(comparisonValue["type"]);
  }
};

const prettyPrintTableName = (tableName: TableName): string => {
  return tableName.map(t => `[${t}]`).join(".");
};

const prettyPrintExistsInTable = (existsInTable: ExistsInTable): string => {
  switch (existsInTable.type) {
    case "related":
      return `RELATED TABLE VIA [${existsInTable.relationship}]`;
    case "unrelated":
      return `UNRELATED TABLE ${prettyPrintTableName(existsInTable.table)}`;
  }
};

export const prettyPrintExpression = (e: Expression): string => {
  switch (e.type) {
    case "and":
      return e.expressions.length
        ? `(${e.expressions.map(prettyPrintExpression).join(" && ")})`
        : "true";
    case "or":
      return e.expressions.length
        ? `(${e.expressions.map(prettyPrintExpression).join(" || ")})`
        : "false";
    case "not":
      return `!(${prettyPrintExpression(e.expression)})`;
    case "exists":
      return `(EXISTS IN ${prettyPrintExistsInTable(e.in_table)} WHERE (${prettyPrintExpression(e.where)}))`
    case "binary_op":
      return `(${prettyPrintComparisonColumn(e.column)} ${prettyPrintBinaryComparisonOperator(e.operator)} ${prettyPrintComparisonValue(e.value)})`;
    case "binary_arr_op":
      return `(${prettyPrintComparisonColumn(e.column)} ${prettyPrintBinaryArrayComparisonOperator(e.operator)} (${e.values.join(", ")}))`;
    case "unary_op":
      return `(${prettyPrintComparisonColumn(e.column)} ${prettyPrintUnaryComparisonOperator(e.operator)})`;
    default:
      return unreachable(e["type"]);
  }
};

const makeFilterPredicate = (
    expression: Expression | null,
    getComparisonColumnValue: (comparisonColumn: ComparisonColumn, row: Record<string, ScalarValue>) => ScalarValue,
    performExistsSubquery: (exists: ExistsExpression, row: Record<string, ScalarValue>) => boolean
  ) => (row: Record<string, ScalarValue>) => {

  const extractComparisonValueScalar = (comparisonValue: ComparisonValue): ScalarValue => {
    switch (comparisonValue.type) {
      case "column":
        return getComparisonColumnValue(comparisonValue.column, row);
      case "scalar":
        return comparisonValue.value;
      default:
        return unreachable(comparisonValue["type"]);
    }
  }

  const evaluate = (e: Expression): boolean => {
    switch (e.type) {
      case "and":
        return reduceAndIterable(mapIterable(e.expressions, evaluate));
      case "or":
        return reduceOrIterable(mapIterable(e.expressions, evaluate));
      case "not":
        return !evaluate(e.expression);
      case "exists":
        return performExistsSubquery(e, row);
      case "binary_op":
        const binOpColumnVal = getComparisonColumnValue(e.column, row);
        const binOpComparisonVal = extractComparisonValueScalar(e.value);
        return getBinaryComparisonOperatorEvaluator(e.operator)(binOpColumnVal, binOpComparisonVal);
      case "binary_arr_op":
        const inColumnVal = getComparisonColumnValue(e.column, row);
        return getBinaryArrayComparisonOperatorEvaluator(e.operator)(inColumnVal, e.values);
      case "unary_op":
        const unOpColumnVal = getComparisonColumnValue(e.column, row);
        return getUnaryComparisonOperatorEvaluator(e.operator)(unOpColumnVal);
      default:
        return unreachable(e["type"]);
    }
  }
  return expression ? evaluate(expression) : true;
};

const makePerformExistsSubquery = (
    findRelationship: (relationshipName: RelationshipName) => Relationship,
    performSubquery: (sourceRow: Record<string, ScalarValue>, tableName: TableName, query: Query) => QueryResponse
  ) => (
    exists: ExistsExpression,
    row: Record<string, ScalarValue>
  ): boolean => {

  const [targetTable, joinExpression] = (() => {
    switch (exists.in_table.type) {
      case "related":
        const relationship = findRelationship(exists.in_table.relationship);
        const joinExpression = createFilterExpressionForRelationshipJoin(row, relationship);
        return [relationship.target_table, joinExpression];
      case "unrelated":
        return [exists.in_table.table, undefined];
      default:
        return unreachable(exists.in_table["type"]);
    }
  })();

  if (joinExpression === null)
    return false;

  const subquery: Query = {
    aggregates: {
      count: { type: "star_count" }
    },
    limit: 1, // We only need one row to exist to satisfy this expresion, this short circuits some filtering
    where: joinExpression !== undefined
      ? { type: "and", expressions: [joinExpression, exists.where] } // Important: the join expression goes first to ensure short circuiting prevents unnecessary subqueries
      : exists.where
  };

  const results = performSubquery(row, targetTable, subquery);
  return (results.aggregates?.count ?? 0) > 0;
}

const buildQueryForPathedOrderByElement = (orderByElement: OrderByElement, orderByRelations: Record<RelationshipName, OrderByRelation>): Query => {
  const [relationshipName, ...remainingPath] = orderByElement.target_path;
  if (relationshipName === undefined) {
    switch (orderByElement.target.type) {
      case "column":
        return {
          fields: {
            [orderByElement.target.column]: { type: "column", column: orderByElement.target.column }
          }
        };
      case "single_column_aggregate":
        return {
          aggregates: {
            [orderByElement.target.column]: { type: "single_column", column: orderByElement.target.column, function: orderByElement.target.function }
          }
        };
      case "star_count_aggregate":
        return {
          aggregates: {
            "count": { type: "star_count" }
          }
        };
      default:
        return unreachable(orderByElement.target["type"]);
    }
  } else {
    const innerOrderByElement = { ...orderByElement, target_path: remainingPath };
    const orderByRelation = orderByRelations[relationshipName];
    const subquery = {
      ...buildQueryForPathedOrderByElement(innerOrderByElement, orderByRelation.subrelations),
      where: orderByRelation.where
    }
    return {
      fields: {
        [relationshipName]: { type: "relationship", relationship: relationshipName, query: subquery }
      }
    };
  }
};

const extractResultFromOrderByElementQueryResponse = (orderByElement: OrderByElement, response: QueryResponse): ScalarValue => {
  const [relationshipName, ...remainingPath] = orderByElement.target_path;
  const rows = response.rows ?? [];
  const aggregates = response.aggregates ?? {};

  if (relationshipName === undefined) {
    switch (orderByElement.target.type) {
      case "column":
        if (rows.length > 1)
          throw new Error(`Unexpected number of rows (${rows.length}) returned by order by element query`);

        const fieldValue = rows.length === 1 ? rows[0][orderByElement.target.column] : null;
        if (fieldValue !== null && typeof fieldValue === "object")
          throw new Error("Column order by target path did not end in a column field value");

        return coerceUndefinedToNull(fieldValue);

      case "single_column_aggregate":
        return aggregates[orderByElement.target.column];

      case "star_count_aggregate":
        return aggregates["count"];

      default:
        return unreachable(orderByElement.target["type"]);
    }
  } else {
    if (rows.length > 1)
      throw new Error(`Unexpected number of rows (${rows.length}) returned by order by element query`);

    const fieldValue = rows.length === 1 ? rows[0][relationshipName] : null;
    if (fieldValue === null || typeof fieldValue !== "object")
      throw new Error(`Found a column field value in the middle of a order by target path: ${orderByElement.target_path}`);

    const innerOrderByElement = { ...orderByElement, target_path: remainingPath };
    return extractResultFromOrderByElementQueryResponse(innerOrderByElement, fieldValue);
  }
};

const makeGetOrderByElementValue = (findRelationship: (relationshipName: RelationshipName) => Relationship, performQuery: (tableName: TableName, query: Query) => QueryResponse) => (orderByElement: OrderByElement, row: Record<string, ScalarValue>, orderByRelations: Record<RelationshipName, OrderByRelation>): ScalarValue => {
  const [relationshipName, ...remainingPath] = orderByElement.target_path;
  if (relationshipName === undefined) {
    if (orderByElement.target.type !== "column")
      throw new Error(`Cannot perform an order by target of type ${orderByElement.target.type} on the current table. Only column-typed targets are supported.`)
    return coerceUndefinedToNull(row[orderByElement.target.column]);
  } else {
    const relationship = findRelationship(relationshipName);
    const orderByRelation = orderByRelations[relationshipName];
    const innerOrderByElement = { ...orderByElement, target_path: remainingPath };
    const query = {
      ...buildQueryForPathedOrderByElement(innerOrderByElement, orderByRelation.subrelations),
      where: orderByRelation.where
    };
    const subquery = addRelationshipFilterToQuery(row, relationship, query);

    if (subquery === null) {
      return null;
    } else {
      const queryResponse = performQuery(relationship.target_table, subquery);
      return extractResultFromOrderByElementQueryResponse(innerOrderByElement, queryResponse);
    }
  }
};

const sortRows = (rows: Record<string, ScalarValue>[], orderBy: OrderBy, getOrderByElementValue: (orderByElement: OrderByElement, row: Record<string, ScalarValue>, orderByRelations: Record<RelationshipName, OrderByRelation>) => ScalarValue): Record<string, ScalarValue>[] =>
  rows
    .map<[Record<string, ScalarValue>, ScalarValue[]]>(row => [row, []])
    .sort(([lhs, lhsValueCache], [rhs, rhsValueCache]) => {
      return orderBy.elements.reduce((accum, orderByElement, orderByElementIndex) => {
        if (accum !== 0) {
          return accum;
        }
        const leftVal: ScalarValue =
          lhsValueCache[orderByElementIndex] !== undefined
            ? lhsValueCache[orderByElementIndex]
            : lhsValueCache[orderByElementIndex] = getOrderByElementValue(orderByElement, lhs, orderBy.relations);
        const rightVal: ScalarValue =
          rhsValueCache[orderByElementIndex] !== undefined
          ? rhsValueCache[orderByElementIndex]
          : rhsValueCache[orderByElementIndex] = getOrderByElementValue(orderByElement, rhs, orderBy.relations);
        const compared =
          leftVal === null
            ? 1
            : rightVal === null
              ? -1
              : leftVal === rightVal
                ? 0
                : leftVal < rightVal
                  ? -1
                  : 1;

        return orderByElement.order_direction === "desc" ? -compared : compared;
      }, 0)
    })
    .map(([row, _valueCache]) => row);

const paginateRows = (rows: Iterable<Record<string, ScalarValue>>, offset: number | null, limit: number | null): Iterable<Record<string, ScalarValue>> => {
  const skipped = offset !== null ? skipIterable(rows, offset) : rows;
  return limit !== null ? takeIterable(skipped, limit) : skipped;
};

const makeFindRelationship = (allTableRelationships: TableRelationships[], tableName: TableName) => (relationshipName: RelationshipName): Relationship => {
  const relationship = allTableRelationships.find(r => tableNameEquals(r.source_table)(tableName))?.relationships?.[relationshipName];
  if (relationship === undefined)
    throw `No relationship named ${relationshipName} found for table ${tableName}`;
  else
    return relationship;
};

const createFilterExpressionForRelationshipJoin = (row: Record<string, ScalarValue>, relationship: Relationship): Expression | null => {
  const columnMappings = Object.entries(relationship.column_mapping);
  const filterConditions: Expression[] = columnMappings
    .map(([outerColumnName, innerColumnName]): [ScalarValue, string] => [row[outerColumnName], innerColumnName])
    .filter((x): x is [ScalarValue, string] => {
      const [outerValue, _] = x;
      return outerValue !== null;
    })
    .map(([outerValue, innerColumnName]) => {
      return {
        type: "binary_op",
        operator: "equal",
        column: {
          path: [],
          name: innerColumnName,
        },
        value: { type: "scalar", value: outerValue }
      };
    });

  if (columnMappings.length === 0 || filterConditions.length !== columnMappings.length) {
    return null;
  } else {
    return { type: "and", expressions: filterConditions }
  }
};

const addRelationshipFilterToQuery = (row: Record<string, ScalarValue>, relationship: Relationship, subquery: Query): Query | null => {
  const filterExpression = createFilterExpressionForRelationshipJoin(row, relationship);

  // If we have no columns to join on, or if some of the FK columns in the row contained null, then we can't join
  if (filterExpression === null) {
    return null;
  } else {
    const existingFilters = subquery.where ? [subquery.where] : []
    return {
      ...subquery,
      limit: relationship.relationship_type === "object" ? 1 : subquery.limit, // If it's an object relationship, we expect only one result to come back, so we can optimise the query by limiting the filtering stop after one row
      where: { type: "and", expressions: [filterExpression, ...existingFilters] }
    };
  }
};

const makeGetComparisonColumnValue = (parentQueryRowChain: Record<string, ScalarValue>[]) => (comparisonColumn: ComparisonColumn, row: Record<string, ScalarValue>): ScalarValue => {
  const path = comparisonColumn.path ?? [];
  if (path.length === 0) {
    return coerceUndefinedToNull(row[comparisonColumn.name]);
  } else if (path.length === 1 && path[0] === "$") {
    const queryRow = parentQueryRowChain.length === 0
      ? row
      : parentQueryRowChain[0];
    return coerceUndefinedToNull(queryRow[comparisonColumn.name]);
  } else {
    throw new Error(`Unsupported path on ComparisonColumn: ${prettyPrintComparisonColumn(comparisonColumn)}`);
  }
};

const projectRow = (fields: Record<string, Field>, findRelationship: (relationshipName: RelationshipName) => Relationship, performQuery: (tableName: TableName, query: Query) => QueryResponse) => (row: Record<string, ScalarValue>): ProjectedRow => {
  const projectedRow: ProjectedRow = {};
  for (const [fieldName, field] of Object.entries(fields)) {

    switch (field.type) {
      case "column":
        projectedRow[fieldName] = coerceUndefinedToNull(row[field.column]);
        break;

      case "relationship":
        const relationship = findRelationship(field.relationship);
        const subquery = addRelationshipFilterToQuery(row, relationship, field.query);
        projectedRow[fieldName] = subquery ? performQuery(relationship.target_table, subquery) : { aggregates: null, rows: null };
        break;

      default:
        return unreachable(field["type"]);
    }
  }
  return projectedRow;
};

const starCountAggregateFunction = (rows: Record<string, ScalarValue>[]): ScalarValue => {
  return rows.length;
};

const columnCountAggregateFunction = (aggregate: ColumnCountAggregate) => (rows: Record<string, ScalarValue>[]): ScalarValue => {
  const nonNullValues = rows.map(row => row[aggregate.column]).filter(v => v !== null);

  return aggregate.distinct
    ? (new Set(nonNullValues)).size
    : nonNullValues.length;
};

const isNumberArray = (values: ScalarValue[]): values is number[] => {
  return values.every(v => typeof v === "number");
};

const isComparableArray = (values: ScalarValue[]): values is (number | string)[] => {
  return values.every(v => typeof v === "number" || typeof v === "string");
};

const singleColumnAggregateFunction = (aggregate: SingleColumnAggregate) => (rows: Record<string, ScalarValue>[]): ScalarValue => {
  const values = rows.map(row => row[aggregate.column]).filter((v): v is Exclude<ScalarValue, null> => v !== null);
  if (values.length === 0)
    return null;

  if (!isComparableArray(values)) {
    throw new Error(`Found non-comparable scalar values when computing ${aggregate.function}`);
  }
  switch (aggregate.function) {
    case "max": return values.reduce((prev, curr) => prev > curr ? prev : curr);
    case "min": return values.reduce((prev, curr) => prev < curr ? prev : curr);
  }

  if (!isNumberArray(values)) {
    throw new Error(`Found non-numeric scalar values when computing ${aggregate.function}`);
  }
  switch (aggregate.function) {
    case "avg":
      return math.mean(values);
    case "stddev_pop": return math.std(values, "uncorrected");
    case "stddev_samp": return math.std(values, "unbiased");
    case "sum": return math.sum(values);
    case "var_pop": return math.variance(values, "uncorrected");
    case "var_samp": return math.variance(values, "unbiased");
    default:
      return unreachable(aggregate.function);
  }
};

const getAggregateFunction = (aggregate: Aggregate): ((rows: Record<string, ScalarValue>[]) => ScalarValue) => {
  switch (aggregate.type) {
    case "star_count":
      return starCountAggregateFunction;
    case "column_count":
      return columnCountAggregateFunction(aggregate);
    case "single_column":
      return singleColumnAggregateFunction(aggregate);
  }
};

const calculateAggregates = (rows: Record<string, ScalarValue>[], aggregateRequest: Record<string, Aggregate>): Record<string, ScalarValue> => {
  return Object.fromEntries(Object.entries(aggregateRequest).map(([fieldName, aggregate]) => {
    const aggregateValue = getAggregateFunction(aggregate)(rows);
    return [fieldName, aggregateValue];
  }));
};

export const queryData = (getTable: (tableName: TableName) => Record<string, ScalarValue>[] | undefined, queryRequest: QueryRequest) => {
  const performQuery = (parentQueryRowChain: Record<string, ScalarValue>[], tableName: TableName, query: Query): QueryResponse => {
    const rows = getTable(tableName);
    if (rows === undefined) {
      throw `${tableName} is not a valid table`;
    }
    const performSubquery = (sourceRow: Record<string, ScalarValue>, tableName: TableName, query: Query): QueryResponse => {
      return performQuery([...parentQueryRowChain, sourceRow], tableName, query);
    };
    const findRelationship = makeFindRelationship(queryRequest.table_relationships, tableName);
    const getComparisonColumnValue = makeGetComparisonColumnValue(parentQueryRowChain);
    const performExistsSubquery = makePerformExistsSubquery(findRelationship, performSubquery);
    const getOrderByElementValue = makeGetOrderByElementValue(findRelationship, performNewQuery);

    const filteredRows = filterIterable(rows, makeFilterPredicate(query.where ?? null, getComparisonColumnValue, performExistsSubquery));
    const sortedRows = query.order_by ? sortRows(Array.from(filteredRows), query.order_by, getOrderByElementValue) : filteredRows;
    const paginatedRows = Array.from(paginateRows(sortedRows, query.offset ?? null, query.limit ?? null));
    const projectedRows = query.fields
      ? paginatedRows.map(projectRow(query.fields, findRelationship, performNewQuery))
      : null;
    const calculatedAggregates = query.aggregates
      ? calculateAggregates(paginatedRows, query.aggregates)
      : null;
    return {
      aggregates: calculatedAggregates,
      rows: projectedRows,
    }
  }
  const performNewQuery = (tableName: TableName, query: Query): QueryResponse => performQuery([], tableName, query);

  return performNewQuery(queryRequest.table, queryRequest.query);
};

const unknownOperator = (x: string): never => { throw new Error(`Unknown operator: ${x}`) };

const expectedString = (x: string): never => { throw new Error(`Expected string value but got ${x}`) };

const expectedNumber = (x: string): never => { throw new Error(`Expected number value but got ${x}`) };
