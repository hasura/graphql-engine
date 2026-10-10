import type { EventTrigger } from '../eventTrigger';
import {
  InsertPermission,
  SelectPermission,
  UpdatePermission,
  DeletePermission,
} from '../permissions';
import type {
  Legacy_SourceToRemoteSchemaRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SourceToSourceRelationship,
  SourceToRemoteSchemaRelationship,
  SameTableObjectRelationship,
} from './relationships';
import { TableFunction } from './source';

/**
 * This represents the type of a table for a datasource as stored in the metadata.
 * With GDC, the type of the table cannot be determined during build time and we can assume the
 * table object to be any valid json representation. The same metadata table metadata object is
 * expected in APIs the server provides when it asks for a table property.
 */
export type Table = SchemaTable | DatasetTable | GDCTable;

export type MetadataTableColumnConfig = {
  custom_name?: string;
  comment?: string;
};

export type MetadataTableConfig = {
  custom_name?: string;
  custom_root_fields?: {
    select?: string;
    select_by_pk?: string;
    select_aggregate?: string;
    select_stream?: string;
    insert?: string;
    insert_one?: string;
    update?: string;
    update_by_pk?: string;
    delete?: string;
    delete_by_pk?: string;
    update_many?: string;
  };
  column_config?: Record<string, MetadataTableColumnConfig>;
  comment?: string;
  logical_model?: string;
  /**
   * @deprecated do not use this anymore. Should be used only for backcompatiblity reasons
   */
  custom_column_names?: Record<string, string>;
};

export type LocalRelationship =
  | {
      type: 'local_object';
      definition: LocalObjectRelationship;
    }
  | {
      type: 'local_array';
      definition: LocalArrayRelationship;
    };

export type LocalArrayRelationship =
  ManualArrayRelationship | LocalTableArrayRelationship;

export type LocalObjectRelationship =
  | ManualObjectRelationship
  | LocalTableObjectRelationship
  | SameTableObjectRelationship;

export type RemoteRelationship =
  | SourceToSourceRelationship
  | SourceToRemoteSchemaRelationship
  | Legacy_SourceToRemoteSchemaRelationship;

export type MetadataTable = {
  /**
   * Table definition
   */
  table: Table;

  /**
   * Table configuration
   */
  configuration?: MetadataTableConfig;

  /**
   * Table relationships
   */
  remote_relationships?: RemoteRelationship[];
  object_relationships?: LocalObjectRelationship[];
  array_relationships?: LocalArrayRelationship[];

  insert_permissions?: InsertPermission[];
  select_permissions?: SelectPermission[];
  update_permissions?: UpdatePermission[];
  delete_permissions?: DeletePermission[];

  /**
   * Event triggers
   */
  event_triggers: EventTrigger[];

  apollo_federation_config?: {
    enable: 'v1';
  } | null;

  is_enum?: boolean;
  computed_fields?: ComputedField[];
};

/**
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/computed-field.html#args-syntax
 */
export type ComputedField = PostgresComputedField | BigqueryComputedField;

/**
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/computed-field.html#args-syntax
 */
export interface PostgresComputedField {
  /**
   * Comment
   */
  comment?: string;
  /**
   * The computed field definition
   */
  definition: PostgresComputedFieldDefinition;
  /**
   * Name of the new computed field
   */
  name: string;
}
/**
 * The computed field definition
 *
 *
 * https://hasura.io/docs/latest/graphql/core/api-reference/schema-metadata-api/computed-field.html#computedfielddefinition
 */
export interface PostgresComputedFieldDefinition {
  /**
   * The SQL function
   */
  function: TableFunction;
  /**
   * Name of the argument which accepts the Hasura session object as a JSON/JSONB value. If
   * omitted, the Hasura session object is not passed to the function
   */
  session_argument?: string;
  /**
   * Name of the argument which accepts a table row type. If omitted, the first argument is
   * considered a table argument
   */
  table_argument?: string;
}

export type BigqueryComputedField = {
  /**
   * Comment
   */
  comment?: string;
  /**
   * The computed field definition
   */
  definition: BigqueryComputedFieldDefinition;
  /**
   * Name of the new computed field
   */
  name: string;
};

export type BigqueryComputedFieldDefinition = {
  /**
   * The user defined SQL function.
   */
  function: DatasetFuntion;
  /**
   * Mapping from the argument name of the function to the column name of the table.
   */
  argument_mapping: Record<string, string>;
  /**
   * Name of the table which the function returns.
   */
  return_table?: DatasetTable;
};

export type SchemaTable = {
  name: string;
  schema: string;
};

export type DatasetFuntion = DatasetTable;

export type DatasetTable = {
  name: string;
  dataset: string;
};

export type QualifiedTable = SchemaTable;

export type DataTarget = SchemaTable & {
  source: string;
};
/**
 * Why is GDCTable as string[] ?
 * It denotes the table along with it's hierarchy based on the DB. For example, in a mysql source
 * you'd have just the table name -> ["Album"] but in a db with schemas -> ["Public", "Album"].
 */
export type GDCTable = string[];
export type GDCFunction = string[];
