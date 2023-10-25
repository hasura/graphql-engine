import {
  InsertPermission,
  SelectPermission,
  UpdatePermission,
  DeletePermission,
} from '../permissions';
import {
  Legacy_SourceToRemoteSchemaRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SourceToSourceRelationship,
  SourceToRemoteSchemaRelationship,
  SameTableObjectRelationship,
} from './relationships';

/**
 * This represents the type of a table for a datasource as stored in the metadata.
 * With GDC, the type of the table cannot be determined during build time and we can assume the
 * table object to be any valid json representation. The same metadata table metadata object is
 * expected in APIs the server provides when it asks for a table property.
 */
export type Table = unknown;

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

export type LocalArrayRelationship =
  | ManualArrayRelationship
  | LocalTableArrayRelationship;

export type LocalObjectRelationship =
  | ManualObjectRelationship
  | LocalTableObjectRelationship
  | SameTableObjectRelationship;

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
  remote_relationships?: (
    | SourceToSourceRelationship
    | SourceToRemoteSchemaRelationship
    | Legacy_SourceToRemoteSchemaRelationship
  )[];
  object_relationships?: LocalObjectRelationship[];
  array_relationships?: LocalArrayRelationship[];

  insert_permissions?: InsertPermission[];
  select_permissions?: SelectPermission[];
  update_permissions?: UpdatePermission[];
  delete_permissions?: DeletePermission[];

  apollo_federation_config?: {
    enable: 'v1';
  } | null;
};
