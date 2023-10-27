import { Source, Table } from '../../../../../hasura-metadata-types';
import { GraphQLType } from 'graphql';
import { Relationship } from '../../../../../DatabaseRelationships';
import { TableColumn } from '../../../../../DataSource';

export type Operators = Record<
  string,
  { label: string; items: Array<{ name: string; value: string }> }
>;

export type Permissions = Record<string, any>;

export type Columns = Pick<
  TableColumn,
  'dataType' | 'name' | 'graphQLProperties'
>[];

export type Relationships = Array<Relationship>;

export type Tables = Array<{
  table: Table;
  columns: Columns;
  relationships: Relationships;
  dataSource: Pick<Source, 'kind' | 'name'> | undefined;
}>;

export type Operator = {
  name: string;
  inputStructure?: string;
  inputType?: string;
  type: string;
  graphqlType?: GraphQLType;
};

export type Comparator = {
  operators: Array<Operator>;
};

export type Comparators = Record<string, Comparator>;

export type PermissionType =
  | 'column'
  | 'exist'
  | 'relationship'
  | 'object'
  | 'value'
  | 'comparator';

export type RowPermissionsState = {
  operators: Operators;
  permissions: Permissions;
  comparators: Comparators;
  setValue: (path: string[], value: any) => void;
  setKey: (props: { path: string[]; key: any; type: PermissionType }) => void;
  setPermissions: (permissions: Permissions) => void;
  loadRelationships?: (relationships: Relationships) => void;
  isLoading?: boolean;
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

export type TableToLoad = { source: string; table: Table }[];
