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
    | SourceToSourceRelationship
    | SourceToRemoteSchemaRelationship
    | Legacy_SourceToRemoteSchemaRelationship
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
