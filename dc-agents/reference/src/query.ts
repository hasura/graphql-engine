import { Expression, Fields, BinaryComparisonOperator, OrderBy, OrderType, ProjectedRow, Query, QueryResponse, RelationshipType, ScalarValue, UnaryComparisonOperator, ComparisonValue, BinaryArrayComparisonOperator, QueryRequest, TableName, ComparisonColumn, TableRelationships, Relationship, RelationshipName } from "./types/query";
import { coerceUndefinedToNull, crossProduct, unreachable, zip } from "./util";

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

      return ordering === OrderType.Descending ? -compared : compared;
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
        operator: BinaryComparisonOperator.Equal,
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

const buildFieldsForPathedComparisonColumn = (comparisonColumn: ComparisonColumn): Fields => {
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

const extractScalarValuesFromFieldPath = (fieldPath: string[], value: ProjectedRow | ScalarValue | ProjectedRow[]): ScalarValue[] => {
  const [fieldName, ...remainingPath] = fieldPath;
  if (fieldName === undefined) {
    if (value !== null && typeof value === "object") { // Yes, the typeof of null and arrays is "object" 😑
      throw "Field path did not end in a ScalarValue";
    } else {
      return [value]; // ScalarValues
    }
  } else {
    if (value === null) { // This can occur with optional object relationships
      return [];
    } else if (Array.isArray(value)) {
      return value.flatMap(row => extractScalarValuesFromFieldPath(fieldPath, row));
    } else if (typeof value === "object") {
      return extractScalarValuesFromFieldPath(remainingPath, value[fieldName]);
    } else {
      throw `Found a ScalarValue in the middle of a field path: ${fieldPath}`;
    }
  }
};

const makeGetComparisonColumnValues = (findRelationship: (relationshipName: RelationshipName) => Relationship, performQuery: (tableName: TableName, query: Query) => ProjectedRow[]) => (comparisonColumn: ComparisonColumn, row: Record<string, ScalarValue>): ScalarValue[] => {
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
      const rows = performQuery(relationship.target_table, subquery);
      const fieldPath = remainingPath.concat(comparisonColumn.name);
      return rows.flatMap(row => extractScalarValuesFromFieldPath(fieldPath, row));
    }
  }
};

const projectRow = (fields: Fields, findRelationship: (relationshipName: RelationshipName) => Relationship, performQuery: (tableName: TableName, query: Query) => ProjectedRow[]) => (row: Record<string, ScalarValue>): ProjectedRow => {
  const projectedRow: ProjectedRow = {};
  for (const [fieldName, field] of Object.entries(fields)) {

    switch (field.type) {
      case "column":
        projectedRow[fieldName] = coerceUndefinedToNull(row[field.column]);
        break;

      case "relationship":
        const relationship = findRelationship(field.relationship);
        const subquery = addRelationshipFilterToQuery(row, relationship, field.query);
        switch (relationship.relationship_type) {
          case RelationshipType.Object:
            projectedRow[fieldName] = subquery ? coerceUndefinedToNull(performQuery(relationship.target_table, subquery)[0]) : null;
            break;

          case RelationshipType.Array:
            projectedRow[fieldName] = subquery ? performQuery(relationship.target_table, subquery) : [];
            break;

          default:
            unreachable(relationship.relationship_type);
            break;
        }
        break;

      default:
        return unreachable(field["type"]);
    }
  }
  return projectedRow;
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
    const slicedRows = paginateRows(sortedRows, query.offset ?? null, query.limit ?? null);
    return slicedRows.map(projectRow(query.fields, findRelationship, performQuery));
  }

  return performQuery(queryRequest.table, queryRequest.query);
};
