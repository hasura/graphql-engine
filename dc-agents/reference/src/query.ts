import { Expression, Fields, BinaryComparisonOperator, OrderBy, OrderType, ProjectedRow, Query, QueryResponse, RelType, RelationshipField, ScalarValue, UnaryComparisonOperator, ComparisonValue, BinaryArrayComparisonOperator } from "./types/query";
import { coerceUndefinedToNull, unreachable } from "./util";

type StaticData = {
  [tableName: string]: Record<string, ScalarValue>[]
}

const prettyPrintBinaryComparisonOperator = (operator: BinaryComparisonOperator): string => {
  switch (operator) {
    case BinaryComparisonOperator.GreaterThan: return ">";
    case BinaryComparisonOperator.GreaterThanOrEqual: return ">=";
    case BinaryComparisonOperator.LessThan: return "<";
    case BinaryComparisonOperator.LessThanOrEqual: return "<=";
    case BinaryComparisonOperator.Equal: return "==";
    default: return unreachable(operator);
  };
};

const prettyPrintBinaryArrayComparisonOperator = (operator: BinaryArrayComparisonOperator): string => {
  switch (operator) {
    case BinaryArrayComparisonOperator.In: return "IN";
    default: return unreachable(operator);
  };
};

const prettyPrintUnaryComparisonOperator = (operator: UnaryComparisonOperator): string => {
  switch (operator) {
    case UnaryComparisonOperator.IsNull: return "IS NULL";
    default: return unreachable(operator);
  };
};

const getBinaryComparisonOperatorEvaluator = (operator: BinaryComparisonOperator): ((left: ScalarValue, right: ScalarValue) => boolean) => {
  switch (operator) {
    case BinaryComparisonOperator.GreaterThan: return (a, b) => a !== null && b !== null && a > b;
    case BinaryComparisonOperator.GreaterThanOrEqual: return (a, b) => a !== null && b !== null && a >= b;
    case BinaryComparisonOperator.LessThan: return (a, b) => a !== null && b !== null && a < b;
    case BinaryComparisonOperator.LessThanOrEqual: return (a, b) => a !== null && b !== null && a <= b;
    case BinaryComparisonOperator.Equal: return (a, b) => a !== null && b !== null && a === b;
    default: return unreachable(operator);
  };
};

const getBinaryArrayComparisonOperatorEvaluator = (operator: BinaryArrayComparisonOperator): ((left: ScalarValue, right: ScalarValue[]) => boolean) => {
  switch (operator) {
    case BinaryArrayComparisonOperator.In: return (a, bs) => a !== null && bs.includes(a);
    default: return unreachable(operator);
  };
};


const getUnaryComparisonOperatorEvaluator = (operator: UnaryComparisonOperator): ((value: ScalarValue) => boolean) => {
  switch (operator) {
    case UnaryComparisonOperator.IsNull: return (v) => v === null;
    default: return unreachable(operator);
  };
};

const prettyPrintScalarComparisonValue = (comparisonValue: ComparisonValue): string => {
  switch (comparisonValue.type) {
    case "column":
      return `[${comparisonValue.column}]`;
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
      return `([${e.column}] ${prettyPrintBinaryComparisonOperator(e.operator)} ${prettyPrintScalarComparisonValue(e.value)})`;
    case "binary_arr_op":
      return `([${e.column}] ${prettyPrintBinaryArrayComparisonOperator(e.operator)} (${e.values.map(prettyPrintScalarComparisonValue).join(", ")}))`;
    case "unary_op":
      return `([${e.column}] ${prettyPrintUnaryComparisonOperator(e.operator)})`;
    default:
      return unreachable(e["type"]);
  }
};

const makeFilterPredicate = (expression: Expression | null) => (row: Record<string, ScalarValue>) => {
  const extractScalarComparisonValue = (comparisonValue: ComparisonValue): ScalarValue => {
    switch (comparisonValue.type) {
      case "column":
        return coerceUndefinedToNull(row[comparisonValue.column]);
      case "scalar":
        return comparisonValue.value;
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
        const binOpColumnVal = coerceUndefinedToNull(row[e.column]);
        return getBinaryComparisonOperatorEvaluator(e.operator)(binOpColumnVal, extractScalarComparisonValue(e.value));
      case "binary_arr_op":
        const inColumnVal = coerceUndefinedToNull(row[e.column]);
        return getBinaryArrayComparisonOperatorEvaluator(e.operator)(inColumnVal, e.values.map(extractScalarComparisonValue));
      case "unary_op":
        const unOpColumnVal = coerceUndefinedToNull(row[e.column]);
        return getUnaryComparisonOperatorEvaluator(e.operator)(unOpColumnVal);
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

      return ordering === OrderType.Descending ? -compared : compared;
    }, 0)
  );

const paginateRows = (rows: Record<string, ScalarValue>[], offset: number | null, limit: number | null): Record<string, ScalarValue>[] => {
  const start = offset ?? 0;
  const end = limit ? start + limit : rows.length;
  return rows.slice(start, end);
};

const createSubqueryForRelationshipField = (row: Record<string, ScalarValue>, field: RelationshipField): Query | null => {
  const columnMappings = Object.entries(field.column_mapping);
  const filterConditions: Expression[] = columnMappings
    .map(([fkName, pkName]): [string, ScalarValue] => [pkName, row[fkName]])
    .filter((x): x is [string, ScalarValue] => {
      const [_, fkVal] = x;
      return fkVal !== null;
    })
    .map(([pkName, fkVal]) => {
      return {
        type: "binary_op",
        operator: BinaryComparisonOperator.Equal,
        column: pkName,
        value: { type: "scalar", value: fkVal }
      };
    });

  // If we have no columns to join on, or if some of the FK columns in the row contained null, then we can't join
  if (columnMappings.length === 0 || filterConditions.length !== columnMappings.length) {
    return null;
  } else {
    const existingFilters = field.query.where ? [field.query.where] : []
    return {
      ...field.query,
      where: { type: "and", expressions: [...filterConditions, ...existingFilters] }
    };
  }
};

const projectRow = (fields: Fields, performQuery: (query: Query) => ProjectedRow[]) => (row: Record<string, ScalarValue>): ProjectedRow => {
  const projectedRow: ProjectedRow = {};
  for (const [fieldName, field] of Object.entries(fields)) {

    switch (field.type) {
      case "column":
        projectedRow[fieldName] = coerceUndefinedToNull(row[field.column]);
        break;

      case "relationship":
        const subquery = createSubqueryForRelationshipField(row, field);
        switch (field.relation_type) {
          case "object":
            projectedRow[fieldName] = subquery ? performQuery(subquery)[0] : null;
            break;

          case "array":
            projectedRow[fieldName] = subquery ? performQuery(subquery) : [];
            break;

          default:
            unreachable(field.relation_type);
            break;
        }
        break;

      default:
        return unreachable(field["type"]);
    }
  }
  return projectedRow;
};

export const queryData = (staticData: StaticData) => {
  const performQuery = (query: Query): QueryResponse => {
    const rows = staticData[query.from];
    if (rows === undefined) {
      throw `${query.from} is not a valid table`;
    }
    const filteredRows = rows.filter(makeFilterPredicate(query.where ?? null));
    const sortedRows = sortRows(filteredRows, query.order_by ?? []);
    const slicedRows = paginateRows(sortedRows, query.offset ?? null, query.limit ?? null);
    return slicedRows.map(projectRow(query.fields, performQuery));
  }

  return performQuery;
};
