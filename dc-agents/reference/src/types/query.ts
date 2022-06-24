export type QueryRequest = {
  table: TableName,
  table_relationships: TableRelationships[],
  query: Query,
}

export type TableName = string

export type TableRelationships = {
  source_table: TableName,
  relationships: { [relationshipName: RelationshipName]: Relationship }
}

export type Relationship = {
  target_table: TableName,
  relationship_type: RelationshipType,
  column_mapping: { [source: SourceColumnName]: TargetColumnName },
}

export type SourceColumnName = ColumnName
export type TargetColumnName = ColumnName

export type RelationshipName = string

export enum RelationshipType {
  Object = "object",
  Array = "array"
}

export type Query = {
  fields: Fields,
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

export type RelationshipField = {
  type: "relationship",
  relationship: RelationshipName
  query: Query,
}

export type ScalarValue = string | number | boolean | null

export type ComparisonColumn = {
  path: RelationshipName[],
  name: ColumnName,
}

export type ComparisonValue =
  | AnotherColumnComparisonValue
  | ScalarComparisonValue

export type AnotherColumnComparisonValue = {
  type: "column",
  column: ComparisonColumn,
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
  column: ComparisonColumn,
  value: ComparisonValue,
}

export type ApplyBinaryArrayComparisonOperatorExpression = {
  type: "binary_arr_op",
  operator: BinaryArrayComparisonOperator,
  column: ComparisonColumn,
  values: ScalarValue[],
}

export type ApplyUnaryComparisonOperatorExpression = {
  type: "unary_op",
  operator: UnaryComparisonOperator,
  column: ComparisonColumn,
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
