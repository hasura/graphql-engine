import { NetworkArgs } from './api';

export type Ref = { $ref: string };

export type OneOf = { oneOf: (Property | Ref)[]; description?: string };

export type Property = {
  description?: string;
  nullable: boolean;
} & (
  | {
      type: 'object';
      properties: Record<string, Ref | Property | OneOf>;
    }
  | {
      type: 'string';
      enum?: string[];
    }
  | {
      type: 'number';
    }
  | {
      type: 'boolean';
    }
);

// export type supportedDrivers = 'postgres' | 'mssql' | 'bigquery' | 'citus';
export type SupportedDrivers =
  | 'postgres'
  | 'bigquery'
  | 'mssql'
  | 'citus'
  | 'gdc';

// This is the new Metadata type that we need to keep updating
export type MetadataTable = {
  table: Record<string, string>;
  configuration?: {
    custom_root_fields?: {
      select?: string;
      select_by_pk?: string;
      select_aggregate?: string;
      insert?: string;
      insert_one?: string;
      update?: string;
      update_by_pk?: string;
      delete?: string;
      delete_by_pk?: string;
    };
    column_config?: Record<string, { custom_name: string; comment: string }>;
    comment?: string;
  };
};

export type Source = {
  name: string;
  kind: SupportedDrivers;
  tables: MetadataTable[];
};

export type Metadata = {
  version: number;
  sources: Source[];
};

type SqlTable = {
  name: string;
  schema: string;
};

type BigQueryTable = {
  name: string;
  dataset: string;
};

export type IntrospectedTable = {
  name: string;
  table: SqlTable | BigQueryTable;
  type: string;
};

export type TableColumn = {
  name: string;
  dataType: string;
};

/**
 * This represents the type of a table for a datasource as stored in the metadata.
 * With GDC, the type of the table cannot be determined during build time and we can assume the
 * table object to be any valid json representation. The same metadata table metadata object is
 * expected in APIs the server provides when it asks for a table property.
 */
export type Table = unknown;

export type GetTrackableTablesProps = {
  dataSourceName: string;
  configuration: any;
} & NetworkArgs;
export type GetTableColumnsProps = {
  dataSourceName: string;
  table: Table;
} & NetworkArgs;
