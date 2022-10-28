import {
  Legacy_SourceToRemoteSchemaRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SameTableObjectRelationship,
  Source,
  SourceToRemoteSchemaRelationship,
  SourceToSourceRelationship,
  SupportedDrivers,
  Table,
} from '@/features/MetadataAPI';

import { NetworkArgs } from './api';

export type { BigQueryTable } from './bigquery';
export { NetworkArgs };

export type AllowedTableRelationships =
  /**
   * Object relationships between columns of the same table. There is no same-table arr relationships
   */
  | SameTableObjectRelationship
  /**
   * Object relationships between columns of two different tables but the tables are in the same DB
   */
  | LocalTableObjectRelationship
  /**
   * Array relationships between columns of two different tables but the tables are in the same DB using FKs
   */
  | LocalTableArrayRelationship
  /**
   * Manually added Object relationships between columns of two different tables but the tables are in the same DB FKs
   */
  | ManualObjectRelationship
  /**
   * Manually added Array relationships between columns of two different tables but the tables are in the same DB
   */
  | ManualArrayRelationship
  /**
   * Manually added relationships between columns of two different tables and the tables are in different DBs
   */
  | SourceToSourceRelationship
  /**
   * Manually added relationships between a DB and a remote schema - there are two formats as per the server.
   */
  | Legacy_SourceToRemoteSchemaRelationship
  | SourceToRemoteSchemaRelationship;

export type IntrospectedTable = {
  name: string;
  table: Table;
  type: string;
};

export type TableColumn = {
  name: string;
  dataType: string;
  nullable?: boolean;
  isPrimaryKey?: boolean;
  graphQLProperties?: {
    name: string;
    scalarType: string;
  };
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
    where?: WhereClause[];
    offset?: number;
    limit?: number;
    order_by?: OrderBy[];
  };
} & NetworkArgs;
export type TableRow = Record<string, unknown>;

export type validOperators = string;
type columnName = string;
export type SelectColumn = string | { name: string; columns: SelectColumn[] };
export type WhereClause = Record<
  columnName,
  Record<validOperators, string | number | boolean>
>;
export type OrderByType = 'asc' | 'desc';
export type OrderByNulls = 'first' | 'last';
export type OrderBy = {
  column: string;
  type: OrderByType;
  nulls?: OrderByNulls;
};

export type Operator = {
  name: string;
  value: string;
  defaultValue?: string;
};
export type GetSupportedOperatorsProps = NetworkArgs;
