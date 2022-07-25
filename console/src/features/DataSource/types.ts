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
export type Table = {
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
  tables: Table[];
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
