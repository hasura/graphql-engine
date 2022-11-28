/**
 * THIS IS A WORK IN PROGRESS
 */

export interface IntegerColumnType {
  int2: number;
  int4: number;
  int8: number;
  smallint: number;
  int: number;
  bigint: number;
}

export interface PrecisionColumnType {
  real: number;
  float4: number;
  float: number;
  float8: number;
  numeric: number;
  decimal: number;
}

export interface SerialColumnType {
  smallserial: number;
  serial: number;
  bigserial: number;
}

export interface StringColumnType {
  uuid: string;
  text: string;
  varchar: string;
  char: string;
  citext: string;
}

export interface BooleanColumnType {
  bit: boolean;
  bool: boolean;
  boolean: boolean;
}

export interface DateTimeColumnType {
  date: string;
  timestamp: string;
  timestamptz: string;
  time: string;
  timetz: string;
  interval: string;
}

export interface NetworkAddressColumnType {
  inet: string;
  cidr: string;
  macaddr: string;
  macaddr8: string;
}

export interface MiscellaneousColumnType {
  money: number;
}

export interface JSONColumnType {
  json: Record<string, unknown>;
  jsonb: Record<string, unknown>;
}

export type JSONColumn = keyof JSONColumnType;

export interface ByteColumnType {
  bytea: string;
}

export type IntegerColumn = keyof IntegerColumnType;
export type PrecisionColumn = keyof PrecisionColumnType;
export type SerialColumn = keyof SerialColumnType;
export type StringColumn = keyof StringColumnType;
export type BooleanColumn = keyof BooleanColumnType;
export type DateTimeColumn = keyof DateTimeColumnType;
export type NetworkAddressColumn = keyof NetworkAddressColumnType;
export type MiscellaneousColumn = keyof MiscellaneousColumnType;
export type ByteColumn = keyof ByteColumnType;

export type Column =
  | IntegerColumn
  | PrecisionColumn
  | SerialColumn
  | StringColumn
  | BooleanColumn
  | DateTimeColumn
  | NetworkAddressColumn
  | MiscellaneousColumn
  | JSONColumn
  | ByteColumn;
