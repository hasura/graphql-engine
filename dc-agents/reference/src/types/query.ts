export type Query = {
  fields: Fields,
  from: string,
  limit?: number | null,
  offset?: number | null,
  where?: Expression | null,
  order_by?: OrderBy[],
}

export type Fields = { [fieldName: string]: Field }

export type Field = ColumnField | RelationshipField

export type ColumnName = string
export type ColumnField = {
  type: "column",
  column: ColumnName,
}

export type PrimaryKey = ColumnName
export type ForeignKey = ColumnName

export type RelType = "object" | "array"

export type RelationshipField = {
  type: "relationship",
  column_mapping: { [primaryKey: PrimaryKey]: ForeignKey },
  relation_type: RelType,
  query: Query,
}

export type ScalarValue = string | number | boolean | null

export type ComparisonValue =
  | AnotherColumnComparisonValue
  | ScalarComparisonValue

export type AnotherColumnComparisonValue = {
  type: "column",
  column: ColumnName,
}

export type ScalarComparisonValue = {
  type: "scalar",
  value: ScalarValue,
}

export type Expression =
  | AndExpression
  | OrExpression
  | NotExpression
  | ApplyBinaryComparisonOperatorExpression
  | ApplyBinaryArrayComparisonOperatorExpression
  | ApplyUnaryComparisonOperatorExpression

export type AndExpression = {
  type: "and",
  expressions: Expression[],
}

export type OrExpression = {
  type: "or",
  expressions: Expression[],
}

export type NotExpression = {
  type: "not",
  expression: Expression,
}

export type ApplyBinaryComparisonOperatorExpression = {
  type: "binary_op",
  operator: BinaryComparisonOperator,
  column: ColumnName,
  value: ComparisonValue,
}

export type ApplyBinaryArrayComparisonOperatorExpression = {
  type: "binary_arr_op",
  operator: BinaryArrayComparisonOperator,
  column: ColumnName,
  values: ComparisonValue[],
}

export type ApplyUnaryComparisonOperatorExpression = {
  type: "unary_op",
  operator: UnaryComparisonOperator,
  column: ColumnName,
}

export enum BinaryComparisonOperator {
  LessThan = "less_than",
  LessThanOrEqual = "less_than_or_equal",
  GreaterThan = "greater_than",
  GreaterThanOrEqual = "greater_than_or_equal",
  Equal = "equal",
}

export enum BinaryArrayComparisonOperator {
  In = "in",
}

export enum UnaryComparisonOperator {
  IsNull = "is_null",
}

export type OrderBy = {
  column: ColumnName,
  ordering: OrderType,
}

export enum OrderType {
  Ascending = "asc",
  Descending = "desc",
}

export type QueryResponse = ProjectedRow[]

export type ProjectedRow = {
  [fieldName: string]: ScalarValue | ProjectedRow[] | ProjectedRow
}
