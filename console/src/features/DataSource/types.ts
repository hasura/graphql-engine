import { Driver } from '@/dataSources';
import { NetworkArgs } from './api';
import { RemoteField } from '../RemoteRelationships';

export type Ref = { $ref: string };

export type OneOf = { oneOf: (Property | Ref)[]; description?: string };

export const isFreeFormObjectField = (
  property: Property & { type: 'object' }
): property is Property & {
  type: 'object';
  additionalProperties: true;
} => {
  if (!('additionalProperties' in property)) return false;

  // eslint-disable-next-line @typescript-eslint/no-unused-vars
  const additionalProperties = property.additionalProperties;

  return true;
};

export type Property = {
  description?: string;
  nullable?: boolean;
} & (
  | {
      type: 'object';
      properties: Record<string, Ref | Property | OneOf>;
    }
  | {
      type: 'object';
      additionalProperties: true;
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
  | {
      type: 'array';
      items:
        | { type: 'string' | 'number' }
        | {
            type: 'object';
            properties: Record<string, Ref | Property | OneOf>;
          }
        | {
            type: 'object';
            additionalProperties: true;
            nullable?: boolean;
          }
        | Ref;
    }
);

// export type supportedDrivers = 'postgres' | 'mssql' | 'bigquery' | 'citus' | 'cockroach' | 'gdc';
export type SupportedDrivers =
  | 'postgres'
  | 'bigquery'
  | 'mssql'
  | 'citus'
  | 'cockroach'
  | 'gdc';

export type RemoteDBRelationship = {
  name: string;
  definition: {
    to_source: {
      relationship_type: 'object' | 'array';
      field_mapping: Record<string, string>;
      source: string;
      table: Table;
    };
  };
};

export type RemoteSchemaRelationship = {
  name: string;
  definition: {
    to_remote_schema: {
      remote_field: RemoteField;
      lhs_fields: string[];
      remote_schema: string;
    };
  };
};

export type LegacyRemoteSchemaRelationship = {
  name: string;
  definition: {
    hasura_fields: string[];
    remote_field: RemoteField;
    remote_schema: string;
  };
};

/**
 * Type based on doc - https://hasura.io/docs/latest/api-reference/metadata-api/relationship/#metadata-pg-create-object-relationship
 */
type BaseLocalRelationshipProps = {
  name: string;
  comment?: string;
};

export type SameTableObjectRelationship = BaseLocalRelationshipProps & {
  using: {
    /**
     * Using foreign key constraint on same table, the string[] is basically the mappable column(s)
     */
    foreign_key_constraint_on: string[] | string;
  };
};

export type LocalTableObjectRelationship = BaseLocalRelationshipProps & {
  using: {
    /**
     * Using foreign key constraint on another table (within the same DB)
     */
    foreign_key_constraint_on: {
      table: Table;
    } & (
      | {
          // Recommened type to use
          columns: string[];
        }
      | {
          // Legacy type < v2.0.10
          column: string;
        }
    );
  };
};

export type ManualObjectRelationship = BaseLocalRelationshipProps & {
  using: {
    /**
     * Manually create a relationship when FK relationships are not available. For eg. BigQuery does not have the concept of FKs, creating
     * relationships manually is the only way to "relate" data points
     */
    manual_configuration: {
      remote_table: Table;
      column_mapping: Record<string, string>;
      insertion_order?: 'before_parent' | 'after_parent' | null;
    };
  };
};

export type LocalTableArrayRelationship = BaseLocalRelationshipProps & {
  using: {
    /**
     * Using foreign key constraint on another table (within the same DB)
     */
    foreign_key_constraint_on: {
      table: Table;
    } & (
      | {
          // Recommened type to use
          columns: string[];
        }
      | {
          // Legacy type < v2.0.10
          column: string;
        }
    );
  };
};

export type ManualArrayRelationship = BaseLocalRelationshipProps & {
  using: {
    /**
     * Manually create a relationship when FK relationships are not available. For eg. BigQuery does not have the concept of FKs, creating
     * relationships manually is the only way to "relate" data points
     */
    manual_configuration: {
      remote_table: Table;
      column_mapping: Record<string, string>;
    };
  };
};

// This is the new Metadata type that we need to keep updating
export type MetadataTable = {
  /**
   * Table definition
   */
  table: Table;

  /**
   * Table configuration
   */
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

  /**
   * Table relationships
   */
  remote_relationships?: (
    | RemoteDBRelationship
    | RemoteSchemaRelationship
    | LegacyRemoteSchemaRelationship
  )[];
  object_relationships?: (
    | ManualObjectRelationship
    | LocalTableObjectRelationship
    | SameTableObjectRelationship
  )[];
  array_relationships?: (
    | ManualArrayRelationship
    | LocalTableArrayRelationship
  )[];
};

export type AllowedTableRelationships =
  | RemoteDBRelationship
  | RemoteSchemaRelationship
  | LegacyRemoteSchemaRelationship
  | ManualObjectRelationship
  | LocalTableObjectRelationship
  | SameTableObjectRelationship
  | ManualArrayRelationship
  | LocalTableArrayRelationship;

export type Source = {
  name: string;
  kind: SupportedDrivers;
  tables: MetadataTable[];
};

export type Metadata = {
  resource_version: number;
  metadata: {
    version: number;
    sources: Source[];
    backend_configs?: {
      dataconnector: Record<string, { uri: string }>;
    };
  };
};

export type IntrospectedTable = {
  name: string;
  table: Table;
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

type ReleaseType = 'GA' | 'Beta';

export type DriverInfoResponse = {
  name: Driver;
  displayName: string;
  release: ReleaseType;
};

export { NetworkArgs };
