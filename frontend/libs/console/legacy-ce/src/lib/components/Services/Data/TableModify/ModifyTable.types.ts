import { ColumnName } from '../TableCommon/DataTableRowItem.types';

export type CheckConstraintsType = {
  check: string;
  name: string;
};

export type DbFunctionName = string;
export type DbDataType = string;

export type ColumnEdit = Record<
  ColumnName,
  {
    comment: string;
    customFieldName: string;
    default: string;
    display_type_name: string;
    isArrayDataType: boolean;
    isIdentity: boolean;
    isNullable: boolean;
    isOnlyPrimaryKey: boolean | undefined;
    isUnique: boolean;
    name: string;
    pkConstraint: string | undefined;
    schemaName: string;
    tableName: string;
    type: string;
  }
>;

export type ForeignKey = {
  colMappings: { column: string; refColumn: string }[];
  constraintName: string;
  onDelete: string;
  onUpdate: string;
  refSchemaName?: string;
  refTableName?: string;
};

// Types available in a particular group
export type TypeAvailable = string;
// Display Name of a type
export type TypeDisplayName = string;
// Description of a type
export type TypeDescription = string;
// Category the particular type belongs to
export type TypeCategory = string;
