import {
  Legacy_SourceToRemoteSchemaRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SourceToSourceRelationship,
  SourceToRemoteSchemaRelationship,
  SameTableObjectRelationship,
  Table,
  SupportedDrivers,
  Source,
} from '@/features/MetadataAPI';

import { NetworkArgs } from './api';

export type { BigQueryTable } from './bigquery';

export type AllowedTableRelationships =
  | Legacy_SourceToRemoteSchemaRelationship
  | SourceToRemoteSchemaRelationship
  | SourceToSourceRelationship
  | ManualObjectRelationship
  | LocalTableObjectRelationship
  | SameTableObjectRelationship
  | ManualArrayRelationship
  | LocalTableArrayRelationship;

export type IntrospectedTable = {
  name: string;
  table: Table;
  type: string;
};

export type TableColumn = {
  name: string;
  dataType: string;
};

export type GetTrackableTablesProps = {
  dataSourceName: string;
  configuration: any;
} & NetworkArgs;
export type GetTableColumnsProps = {
  dataSourceName: string;
  configuration?: Source['configuration'];
  table: Table;
} & NetworkArgs;
export type GetFKRelationshipProps = {
  dataSourceName: string;
  table: Table;
} & NetworkArgs;

export type TableFkRelationships = {
  from: {
    table: string;
    column: string[];
  };
  to: {
    table: string;
    column: string[];
  };
};

export type GetTablesListAsTreeProps = {
  dataSourceName: string;
} & NetworkArgs;

type ReleaseType = 'GA' | 'Beta' | 'disabled';

export type DriverInfoResponse = {
  name: SupportedDrivers;
  displayName: string;
  release: ReleaseType;
};

export type GetTableRowsProps = {
  table: Table;
  dataSourceName: string;
  columns: string[];
  options?: {
    where?: WhereClause;
    offset?: number;
    limit?: number;
    order_by?: OrderBy[];
  };
} & NetworkArgs;
export type TableRow = Record<string, unknown>;

export type validOperators = string;

export type SelectColumn = string | { name: string; columns: SelectColumn[] };
export type WhereClause = Record<
  validOperators,
  Record<string, string | number | boolean>
>[];
export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';
export type OrderBy = {
  column: string;
  type: OrderByType;
  nulls?: OrderByNulls;
};

export { NetworkArgs };
