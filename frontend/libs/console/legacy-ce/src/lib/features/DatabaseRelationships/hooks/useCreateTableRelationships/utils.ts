import { MetadataError, Table } from '../../../hasura-metadata-types';
import zipObject from 'lodash/zipObject';
import {
  CreateTableRelationshipRequestBodyProps,
  DeleteRelationshipProps,
  RenameRelationshipProps,
} from './types';
import {
  isLocalTableRelationshipDefinition,
  isRemoteSchemaRelationshipDefinition,
  isRemoteTableRelationshipDefinition,
} from './typeGuards';

const createLocalFkRelationshipRequestBody = (props: {
  name: string;
  type: 'array' | 'object';
  driver: string;
  source: string;
  fromTable: Table;
  toTable: Table;
  fkConstraintOn: 'fromTable' | 'toTable';
  fromColumns: string[];
  toColumns: string[];
}) => {
  return {
    type: `${props.driver}_create_${props.type}_relationship`,
    args: {
      table: props.fromTable,
      name: props.name,
      source: props.source,
      using: {
        foreign_key_constraint_on:
          props.fkConstraintOn === 'fromTable'
            ? props.fromColumns
            : {
                table: props.toTable,
                columns: props.toColumns,
              },
      },
    },
  };
};

export const renameRelationshipRequestBody = (
  props: RenameRelationshipProps
) => ({
  type: `${props.driver}_rename_relationship`,
  args: {
    table: props.table,
    name: props.name,
    source: props.source,
    new_name: props.new_name,
  },
});

const createLocalManualRelationship = (props: {
  name: string;
  type: 'array' | 'object';
  driver: string;
  source: string;
  fromTable: Table;
  toTable: Table;
  columnMapping: Record<string, string>;
}) => {
  return {
    type: `${props.driver}_create_${props.type}_relationship`,
    args: {
      table: props.fromTable,
      name: props.name,
      source: props.source,
      using: {
        manual_configuration: {
          remote_table: props.toTable,
          column_mapping: props.columnMapping,
        },
      },
    },
  };
};

const createRemoteTableRelationshipRequestBody = (props: {
  name: string;
  type: 'array' | 'object';
  driver: string;
  fromSource: string;
  toSource: string;
  fromTable: Table;
  toTable: Table;
  columnMapping: Record<string, string>;
  editMode?: boolean;
}) => {
  return {
    type: `${props.driver}_${
      props.editMode ? 'update' : 'create'
    }_remote_relationship`,
    args: {
      name: props.name,
      source: props.fromSource,
      table: props.fromTable,
      definition: {
        to_source: {
          relationship_type: props.type,
          source: props.toSource,
          table: props.toTable,
          field_mapping: props.columnMapping,
        },
      },
    },
  };
};

const createRemoteSchemaRelationshipRequestBody = (props: {
  name: string;
  driver: string;
  fromSource: string;
  fromTable: Table;
  toRemoteSchema: string;
  lhs_fields: string[];
  remote_field: Record<string, any>;
  editMode?: boolean;
}) => {
  return {
    type: `${props.driver}_${
      props.editMode ? 'update' : 'create'
    }_remote_relationship`,
    args: {
      name: props.name,
      source: props.fromSource,
      table: props.fromTable,
      definition: {
        to_remote_schema: {
          remote_schema: props.toRemoteSchema,
          lhs_fields: props.lhs_fields,
          remote_field: props.remote_field,
        },
      },
    },
  };
};

export const createTableRelationshipRequestBody = (
  props: CreateTableRelationshipRequestBodyProps
) => {
  if (isLocalTableRelationshipDefinition(props.definition)) {
    if (props.isEditMode) {
      if (
        !props.sourceCapabilities.isLocalTableRelationshipSupported &&
        props.targetCapabilities.isRemoteTableRelationshipSupported
      ) {
        return createRemoteTableRelationshipRequestBody({
          name: props.name,
          driver: props.driver,
          type: props.definition.type,
          fromSource: props.source.fromSource,
          toSource: props.definition.target.toSource,
          fromTable: props.source.fromTable,
          toTable: props.definition.target.toTable,
          columnMapping:
            'fkConstraintOn' in props.definition.detail
              ? zipObject(
                  props.definition.detail.fromColumns,
                  props.definition.detail.toColumns
                )
              : props.definition.detail.columnMapping,
          editMode: true,
        });
      }
      throw Error('Edit Local Relationship not supported');
    }

    if (props.source.fromSource === props.definition.target.toSource) {
      if (props.sourceCapabilities.isLocalTableRelationshipSupported) {
        return 'fkConstraintOn' in props.definition.detail
          ? createLocalFkRelationshipRequestBody({
              name: props.name,
              driver: props.driver,
              source: props.source.fromSource,
              type: props.definition.type,
              fromTable: props.source.fromTable,
              toTable: props.definition.target.toTable,
              fkConstraintOn: props.definition.detail.fkConstraintOn,
              fromColumns: props.definition.detail.fromColumns,
              toColumns: props.definition.detail.toColumns,
            })
          : createLocalManualRelationship({
              name: props.name,
              driver: props.driver,
              source: props.source.fromSource,
              type: props.definition.type,
              fromTable: props.source.fromTable,
              toTable: props.definition.target.toTable,
              columnMapping: props.definition.detail.columnMapping,
            });
      }

      if (
        !props.sourceCapabilities.isLocalTableRelationshipSupported &&
        props.targetCapabilities.isRemoteTableRelationshipSupported
      ) {
        return createRemoteTableRelationshipRequestBody({
          name: props.name,
          driver: props.driver,
          type: props.definition.type,
          fromSource: props.source.fromSource,
          toSource: props.definition.target.toSource,
          fromTable: props.source.fromTable,
          toTable: props.definition.target.toTable,
          columnMapping:
            'fkConstraintOn' in props.definition.detail
              ? zipObject(
                  props.definition.detail.fromColumns,
                  props.definition.detail.toColumns
                )
              : props.definition.detail.columnMapping,
        });
      }

      throw Error('Local Relationship not supported');
    }
  }

  if (isRemoteTableRelationshipDefinition(props.definition)) {
    if (props.targetCapabilities.isRemoteTableRelationshipSupported) {
      return createRemoteTableRelationshipRequestBody({
        name: props.name,
        driver: props.driver,
        type: props.definition.type,
        fromSource: props.source.fromSource,
        toSource: props.definition.target.toRemoteSource,
        fromTable: props.source.fromTable,
        toTable: props.definition.target.toRemoteTable,
        columnMapping: props.definition.detail.columnMapping,
        editMode: props.isEditMode,
      });
    }

    throw Error('Remote Database Relationship not supported');
  }

  if (isRemoteSchemaRelationshipDefinition(props.definition)) {
    if (props.sourceCapabilities.isRemoteSchemaRelationshipSupported) {
      return createRemoteSchemaRelationshipRequestBody({
        name: props.name,
        driver: props.driver,
        fromSource: props.source.fromSource,
        fromTable: props.source.fromTable,
        toRemoteSchema: props.definition.target.toRemoteSchema,
        lhs_fields: props.definition.detail.lhs_fields,
        remote_field: props.definition.detail.remote_field,
        editMode: props.isEditMode,
      });
    }

    throw Error('Remote Schema Relationship not supported');
  }

  return 'Not implemented';
};

export const deleteTableRelationshipRequestBody = (
  props: DeleteRelationshipProps
) => {
  if (props.isRemote)
    return {
      type: `${props.driver}_delete_remote_relationship`,
      args: {
        source: props.source,
        table: props.table,
        name: props.name,
      },
    };
  return {
    type: `${props.driver}_drop_relationship`,
    args: {
      source: props.source,
      table: props.table,
      relationship: props.name,
    },
  };
};

export const safeParseErrors = (errors: MetadataError[]) => {
  try {
    return errors.map(err => err.error).join('\n');
  } catch (err) {
    return JSON.stringify(errors);
  }
};
