export type SchemaResponse = {
  tables: Table[],
}

export type Table = {
  name: string,
  columns: ColumnInfo[],
  primary_key?: string | null,
  description?: string | null
}

export type ColumnInfo = {
  name: string,
  type: ScalarType,
  nullable: boolean,
  description?: string | null
}

export enum ScalarType {
  String = "string",
  Number = "number",
  Boolean = "bool"
}
