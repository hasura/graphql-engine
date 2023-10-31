import { GraphQLType } from 'graphql';
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
  QualifiedFunction,
} from '../hasura-metadata-types';

import type { NetworkArgs } from './api';
import { ColumnValueGenerationStrategy } from '@hasura/dc-api-types';

export type { BigQueryTable } from './bigquery';
export type { NetworkArgs };

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
  /**
   * Name of the column as defined in the DB
   */
  name: string;
  /**
   * dataType of the column as defined in the DB
   */
  dataType: string | { type: string; name: string };
  /**
   * console data type: the dataType property is group into one of these types and console uses this internally
   */
  consoleDataType: 'string' | 'text' | 'json' | 'number' | 'boolean' | 'float';
  nullable?: boolean;
  isPrimaryKey?: boolean;
  graphQLProperties?: {
    name: string;
    scalarType: string;
    graphQLType?: GraphQLType | undefined;
  };
  value_generated?: ColumnValueGenerationStrategy;
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
    table: Table;
    column: string[];
  };
  to: {
    table: Table;
    column: string[];
  };
};

export type GetTablesListAsTreeProps = {
  dataSourceName: string;
  releaseName?: ReleaseType;
} & NetworkArgs;

export type ReleaseType = 'GA' | 'Beta' | 'Alpha' | 'disabled';

export type DriverInfo = {
  name: SupportedDrivers;
  displayName: string;
  release: ReleaseType;
  native?: boolean;
  available?: boolean;
  enterprise?: boolean;
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
export type TableRow = Record<string, string | number | boolean>;

export type validOperators = string;
type columnName = string;
export type SelectColumn = string | { name: string; columns: SelectColumn[] };
export type WhereClause = Record<
  columnName,
  Record<
    validOperators,
    string | number | boolean | string[] | number[] | boolean[]
  >
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

export type Version = string;
export type GetVersionProps = { dataSourceName: string } & NetworkArgs;
export type InsertRowArgs = {
  dataSourceName: string;
  httpClient: NetworkArgs['httpClient'];
  rowValues: Record<string, unknown>;
  table: Table;
};

export type GetDefaultQueryRootProps = {
  dataSourceName: string;
  table: Table;
};

export type GetTrackableFunctionProps = {
  dataSourceName: string;
} & NetworkArgs;

export type IntrospectedFunction = {
  name: string;
  qualifiedFunction: QualifiedFunction;
  isVolatile: boolean;
};
export type GetDatabaseSchemaProps = {
  dataSourceName: string;
} & NetworkArgs;

export type ChangeDatabaseSchemaProps = {
  dataSourceName: string;
  schemaName: string;
} & NetworkArgs;
export type GetIsTableViewProps = {
  dataSourceName: string;
  table: Table;
  httpClient: NetworkArgs['httpClient'];
};
export type GetSupportedScalarsProps = {
  dataSourceKind: string;
} & NetworkArgs;

export type StoredProcedure = unknown;
export type GetStoredProceduresProps = {
  dataSourceName: string;
} & NetworkArgs;
