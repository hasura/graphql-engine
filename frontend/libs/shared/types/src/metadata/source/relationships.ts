import type { Table } from './table';

export type RemoteField = {
  [FieldName: string]: {
    arguments: InputArgumentsType | never;
    field?: RemoteField;
  };
};

export type InputArgumentValueType =
  string | boolean | number | InputArgumentsType;

export type InputArgumentsType = {
  [key: string]: InputArgumentValueType;
};

// https://hasura.io/docs/2.0/api-reference/syntax-defs/#tosourcerelationshipdefinition
export type SourceToSourceRelationship = {
  name: string;
  definition: {
    to_source: {
      // The type of the relationship.
      relationship_type: 'object' | 'array';
      // Mapping of columns from current table to remote table.
      field_mapping: Record<string, string>;
      // Name of the source of the target table.
      source: string;
      // Name of the target table.
      table: Table;
    };
  };
};

export type SourceToRemoteSchemaRelationship = {
  name: string;
  definition: {
    to_remote_schema: {
      // the join condition - this would generate this upstream request:
      // query {
      //   messages(id: id_from_users_table) { .. }
      // }
      remote_field: RemoteField;
      // the fields on the table that need to be selected to pass the
      // required data to the Remote Schema's resolver.
      lhs_fields: string[];
      // name of the target Remote Schema
      remote_schema: string;
    };
  };
};

export type Legacy_SourceToRemoteSchemaRelationship = {
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
          // Recommended type to use
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
