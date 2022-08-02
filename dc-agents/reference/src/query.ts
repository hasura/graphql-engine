import { QueryRequest, TableRelationships, Relationship, Query, Field, OrderBy, Expression, BinaryComparisonOperator, UnaryComparisonOperator, BinaryArrayComparisonOperator, ComparisonColumn, ComparisonValue, ScalarValue, QueryResponse, Aggregate, SingleColumnAggregate, ColumnCountAggregate } from "./types";
import { coerceUndefinedToNull, crossProduct, unreachable, zip } from "./util";
import * as math from "mathjs";

type StaticData = {
  [tableName: string]: Record<string, ScalarValue>[]
}

type TableName = string
type RelationshipName = string

type ProjectedRow = {
  [fieldName: string]: ScalarValue | QueryResponse
}

const prettyPrintBinaryComparisonOperator = (operator: BinaryComparisonOperator): string => {
  switch (operator) {
    case "greater_than": return ">";
    case "greater_than_or_equal": return ">=";
    case "less_than": return "<";
    case "less_than_or_equal": return "<=";
    case "equal": return "==";
    case "not_equal": return "!="; // Custom operator
    default: return unknownOperator(operator);
  };
};

const prettyPrintBinaryArrayComparisonOperator = (operator: BinaryArrayComparisonOperator): string => {
  switch (operator) {
    case "in": return "IN";
    case "not_in": return "NOT IN"; // Custom operator
    default: return unknownOperator(operator);
  };
};

const prettyPrintUnaryComparisonOperator = (operator: UnaryComparisonOperator): string => {
  switch (operator) {
    case "is_null": return "IS NULL";
    case "is_not_null": return "IS NOT NULL"; // Custom operator
    default: return unknownOperator(operator);
  };
};

const getBinaryComparisonOperatorEvaluator = (operator: BinaryComparisonOperator): ((left: ScalarValue, right: ScalarValue) => boolean) => {
  switch (operator) {
    case "greater_than": return (a, b) => a !== null && b !== null && a > b;
    case "greater_than_or_equal": return (a, b) => a !== null && b !== null && a >= b;
    case "less_than": return (a, b) => a !== null && b !== null && a < b;
    case "less_than_or_equal": return (a, b) => a !== null && b !== null && a <= b;
    case "equal": return (a, b) => a !== null && b !== null && a === b;
    case "not_equal": return (a, b) => a === null || b === null || a !== b; // Custom operator
    default: return unknownOperator(operator);
  };
};

const getBinaryArrayComparisonOperatorEvaluator = (operator: BinaryArrayComparisonOperator): ((left: ScalarValue, right: ScalarValue[]) => boolean) => {
  switch (operator) {
    case "in": return (a, bs) => a !== null && bs.includes(a);
    case "not_in": return (a, bs) => a === null || !bs.includes(a); // Custom operator
    default: return unknownOperator(operator);
  };
};


const getUnaryComparisonOperatorEvaluator = (operator: UnaryComparisonOperator): ((value: ScalarValue) => boolean) => {
  switch (operator) {
    case "is_null": return (v) => v === null;
    case "is_not_null": return (v) => v !== null; // Custom operator
    default: return unknownOperator(operator);
  };
};

const prettyPrintComparisonColumn = (comparisonColumn: ComparisonColumn): string => {
  return comparisonColumn.path.concat(comparisonColumn.name).map(p => `[${p}]`).join(".");
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
}

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
    case "binary_op":
      return `([${prettyPrintComparisonColumn(e.column)}] ${prettyPrintBinaryComparisonOperator(e.operator)} ${prettyPrintComparisonValue(e.value)})`;
    case "binary_arr_op":
      return `([${prettyPrintComparisonColumn(e.column)}] ${prettyPrintBinaryArrayComparisonOperator(e.operator)} (${e.values.join(", ")}))`;
    case "unary_op":
      return `([${prettyPrintComparisonColumn(e.column)}] ${prettyPrintUnaryComparisonOperator(e.operator)})`;
    default:
      return unreachable(e["type"]);
  }
};

const areComparingColumnsOnSameTable = (comparisonColumn: ComparisonColumn, comparisonValue: ComparisonValue): boolean => {
  if (comparisonValue.type === "scalar")
    return false;
  if (comparisonColumn.path.length !== comparisonValue.column.path.length)
    return false;
  return zip(comparisonColumn.path, comparisonValue.column.path).every(([p1, p2]) => p1 === p2);
};

const makeFilterPredicate = (expression: Expression | null, getComparisonColumnValues: (comparisonColumn: ComparisonColumn, row: Record<string, ScalarValue>) => ScalarValue[]) => (row: Record<string, ScalarValue>) => {
  const extractComparisonValueScalars = (comparisonValue: ComparisonValue): ScalarValue[] => {
    switch (comparisonValue.type) {
      case "column":
        return getComparisonColumnValues(comparisonValue.column, row);
      case "scalar":
        return [comparisonValue.value];
      default:
        return unreachable(comparisonValue["type"]);
    }
  }

  const evaluate = (e: Expression): boolean => {
    switch (e.type) {
      case "and":
        return e.expressions.map(evaluate).reduce((b1, b2) => b1 && b2, true);
      case "or":
        return e.expressions.map(evaluate).reduce((b1, b2) => b1 || b2, false);
      case "not":
        return !evaluate(e.expression);
      case "binary_op":
        const binOpColumnVals = getComparisonColumnValues(e.column, row);
        const binOpComparisonVals = extractComparisonValueScalars(e.value);
        const comparisonPairs =
          areComparingColumnsOnSameTable(e.column, e.value)
            ? zip(binOpColumnVals, binOpComparisonVals)
            : crossProduct(binOpColumnVals, binOpComparisonVals);
        return comparisonPairs.some(([columnVal, comparisonVal]) => getBinaryComparisonOperatorEvaluator(e.operator)(columnVal, comparisonVal));
      case "binary_arr_op":
        const inColumnVals = getComparisonColumnValues(e.column, row);
        return inColumnVals.some(columnVal => getBinaryArrayComparisonOperatorEvaluator(e.operator)(columnVal, e.values));
      case "unary_op":
        const unOpColumnVals = getComparisonColumnValues(e.column, row);
        return unOpColumnVals.some(columnVal => getUnaryComparisonOperatorEvaluator(e.operator)(columnVal));
      default:
        return unreachable(e["type"]);
    }
  }
  return expression ? evaluate(expression) : true;
};

const sortRows = (rows: Record<string, ScalarValue>[], orderBy: OrderBy[]): Record<string, ScalarValue>[] =>
  rows.sort((lhs, rhs) =>
    orderBy.reduce((accum, { column, ordering }) => {
      if (accum !== 0) {
        return accum;
      }
      const leftVal: ScalarValue = coerceUndefinedToNull(lhs[column]);
      const rightVal: ScalarValue = coerceUndefinedToNull(rhs[column]);
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

      return ordering === "desc" ? -compared : compared;
    }, 0)
  );

const paginateRows = (rows: Record<string, ScalarValue>[], offset: number | null, limit: number | null): Record<string, ScalarValue>[] => {
  const start = offset ?? 0;
  const end = limit ? start + limit : rows.length;
  return rows.slice(start, end);
};

const makeFindRelationship = (allTableRelationships: TableRelationships[], tableName: TableName) => (relationshipName: RelationshipName): Relationship => {
  const relationship = allTableRelationships.find(r => r.source_table === tableName)?.relationships?.[relationshipName];
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
      where: { type: "and", expressions: [filterExpression, ...existingFilters] }
    };
  }
};

const buildFieldsForPathedComparisonColumn = (comparisonColumn: ComparisonColumn): Record<string, Field> => {
  const [relationshipName, ...remainingPath] = comparisonColumn.path;
  if (relationshipName === undefined) {
    return {
      [comparisonColumn.name]: { type: "column", column: comparisonColumn.name }
    };
  } else {
    const innerComparisonColumn = { ...comparisonColumn, path: remainingPath };
    return {
      [relationshipName]: { type: "relationship", relationship: relationshipName, query: { fields: buildFieldsForPathedComparisonColumn(innerComparisonColumn) } }
    };
  }
};

const extractScalarValuesFromFieldPath = (fieldPath: string[], row: ProjectedRow): ScalarValue[] => {
  const [fieldName, ...remainingPath] = fieldPath;
  const fieldValue = row[fieldName];

  if (remainingPath.length === 0) {
    if (fieldValue === null || typeof fieldValue !== "object") {
      return [fieldValue];
    } else {
      throw new Error("Field path did not end in a column field value");
    }
  } else {
    if (fieldValue !== null && typeof fieldValue === "object") {
      return (fieldValue.rows ?? []).flatMap(row => extractScalarValuesFromFieldPath(remainingPath, row));
    } else {
      throw new Error(`Found a column field value in the middle of a field path: ${fieldPath}`);
    }
  }
};

const makeGetComparisonColumnValues = (findRelationship: (relationshipName: RelationshipName) => Relationship, performQuery: (tableName: TableName, query: Query) => QueryResponse) => (comparisonColumn: ComparisonColumn, row: Record<string, ScalarValue>): ScalarValue[] => {
  const [relationshipName, ...remainingPath] = comparisonColumn.path;
  if (relationshipName === undefined) {
    return [coerceUndefinedToNull(row[comparisonColumn.name])];
  } else {
    const relationship = findRelationship(relationshipName);
    const query: Query = { fields: buildFieldsForPathedComparisonColumn({ ...comparisonColumn, path: remainingPath }) };
    const subquery = addRelationshipFilterToQuery(row, relationship, query);

    if (subquery === null) {
      return [];
    } else {
      const rows = performQuery(relationship.target_table, subquery).rows ?? [];
      const fieldPath = remainingPath.concat(comparisonColumn.name);
      return rows.flatMap(row => extractScalarValuesFromFieldPath(fieldPath, row));
    }
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

export const queryData = (staticData: StaticData, queryRequest: QueryRequest) => {
  const performQuery = (tableName: TableName, query: Query): QueryResponse => {
    const rows = staticData[tableName];
    if (rows === undefined) {
      throw `${tableName} is not a valid table`;
    }
    const findRelationship = makeFindRelationship(queryRequest.table_relationships, tableName);
    const getComparisonColumnValues = makeGetComparisonColumnValues(findRelationship, performQuery);

    const filteredRows = rows.filter(makeFilterPredicate(query.where ?? null, getComparisonColumnValues));
    const sortedRows = sortRows(filteredRows, query.order_by ?? []);
    const paginatedRows = paginateRows(sortedRows, query.offset ?? null, query.limit ?? null);
    const projectedRows = query.fields
      ? paginatedRows.map(projectRow(query.fields, findRelationship, performQuery))
      : null;
    const calculatedAggregates = query.aggregates
      ? calculateAggregates(paginatedRows, query.aggregates)
      : null;
    return {
      aggregates: calculatedAggregates,
      rows: projectedRows,
    }
  }

  return performQuery(queryRequest.table, queryRequest.query);
};

const unknownOperator = (x: string): never => { throw new Error(`Unknown operator: ${x}`) };
