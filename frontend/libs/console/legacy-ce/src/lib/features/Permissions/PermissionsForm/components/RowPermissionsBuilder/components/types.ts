import { Table } from '@/features/hasura-metadata-types';
import { GraphQLType } from 'graphql';

export type Operators = Record<
  string,
  { label: string; items: Array<{ name: string; value: string }> }
>;

export type Permissions = Record<string, any>;

export type Columns = Array<{
  name: string;
  type: string;
  graphQLType: GraphQLType;
}>;

export type Relationships = Array<{
  name: string;
  table: Table;
  type: 'object' | 'array';
}>;

export type Tables = Array<{
  table: Table;
  columns: Columns;
  relationships: Relationships;
}>;

export type Comparator = {
  operators: Array<{
    name: string;
    operator: string;
    defaultValue?: string;
    type: GraphQLType;
  }>;
};

export type Comparators = Record<string, Comparator>;

export type PermissionType =
  | 'column'
  | 'exist'
  | 'relationship'
  | 'value'
  | 'comparator';

export type RowPermissionsState = {
  operators: Operators;
  permissions: Permissions;
  comparators: Comparators;
  table: Table;
  tables: Tables;
  setValue: (path: string[], value: any) => void;
  setKey: (props: { path: string[]; key: any; type: PermissionType }) => void;
  setPermissions: (permissions: Permissions) => void;
};

export type TypesContext = {
  types: Record<string, { type: PermissionType }>;
  setType: ({
    type,
    path,
    value,
  }: {
    type: PermissionType;
    path: string[];
    value: any;
  }) => void;
};

export type TableContext = {
  table: Table;
  setTable: (table: Table) => void;
  comparator?: string | undefined;
  setComparator: (comparator: string | undefined) => void;
  columns: Columns;
  setColumns: (columns: Columns) => void;
  relationships: Relationships;
  setRelationships: (relationships: Relationships) => void;
};
