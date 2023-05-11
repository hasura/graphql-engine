import { isSameTableObjectRelationship } from '../../DataSource';
import { areTablesEqual } from '../../hasura-metadata-api';
import {
  Legacy_SourceToRemoteSchemaRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SameTableObjectRelationship,
  SourceToRemoteSchemaRelationship,
  SourceToSourceRelationship,
  Table,
} from '../../hasura-metadata-types';
import isEqual from 'lodash/isEqual';
import {
  LocalRelationship,
  RemoteDatabaseRelationship,
  RemoteSchemaRelationship,
  SuggestedRelationship,
} from '../types';

const getKeyValuePair = (arr1: string[], arr2: string[]) => {
  const result: Record<string, string> = {};
  for (let i = 0; i < arr1.length; i++) {
    result[arr1[i]] = arr2[i];
  }
  return result;
};

const getFkDefinition = (
  table: Table,
  relationship:
    | SameTableObjectRelationship
    | LocalTableObjectRelationship
    | LocalTableArrayRelationship,
  suggestedRelationships: SuggestedRelationship[]
): { toTable: Table; mapping: Record<string, string> } => {
  if (isSameTableObjectRelationship(relationship)) {
    const fromTable = table;
    const fromColumns = Array.isArray(
      relationship.using.foreign_key_constraint_on
    )
      ? relationship.using.foreign_key_constraint_on
      : [relationship.using.foreign_key_constraint_on];

    const matchingFkConstraint = suggestedRelationships.find(
      suggestedRelationship =>
        areTablesEqual(fromTable, suggestedRelationship.from.table) &&
        isEqual(fromColumns.sort(), suggestedRelationship.from.columns)
    );

    return {
      toTable: matchingFkConstraint?.to.table,
      mapping: matchingFkConstraint
        ? getKeyValuePair(
            matchingFkConstraint.from.columns,
            matchingFkConstraint.to.columns
          )
        : {},
    };
  }

  const toTable = table;
  const toColumn =
    'column' in relationship.using.foreign_key_constraint_on
      ? [relationship.using.foreign_key_constraint_on.column]
      : relationship.using.foreign_key_constraint_on.columns;

  const matchingFkConstraint = suggestedRelationships.find(
    suggestedRelationship =>
      areTablesEqual(toTable, suggestedRelationship.to.table) &&
      isEqual(toColumn.sort(), suggestedRelationship.to.columns)
  );

  return {
    toTable: matchingFkConstraint?.from.table,
    mapping: matchingFkConstraint
      ? getKeyValuePair(
          matchingFkConstraint.to.columns,
          matchingFkConstraint.from.columns
        )
      : {},
  };
};

export const adaptLocalObjectRelationshipWithManualConfiguration = ({
  table,
  dataSourceName,
  relationship,
}: {
  table: Table;
  dataSourceName: string;
  relationship: ManualObjectRelationship;
}): LocalRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType: 'Object',
    type: 'localRelationship',
    definition: {
      toTable: relationship.using.manual_configuration.remote_table,
      mapping: relationship.using.manual_configuration.column_mapping,
    },
  };
};

export const adaptLocalObjectRelationshipWithFkConstraint = ({
  table,
  dataSourceName,
  relationship,
  suggestedRelationships,
}: {
  table: Table;
  dataSourceName: string;
  relationship: SameTableObjectRelationship | LocalTableObjectRelationship;
  suggestedRelationships: SuggestedRelationship[];
}): LocalRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType: 'Object',
    type: 'localRelationship',
    definition: getFkDefinition(table, relationship, suggestedRelationships),
  };
};

export const adaptLocalArrayRelationshipWithManualConfiguration = ({
  table,
  dataSourceName,
  relationship,
}: {
  table: Table;
  dataSourceName: string;
  relationship: ManualArrayRelationship;
}): LocalRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType: 'Array',
    type: 'localRelationship',
    definition: {
      toTable: relationship.using.manual_configuration.remote_table,
      mapping: relationship.using.manual_configuration.column_mapping,
    },
  };
};

export const adaptLocalArrayRelationshipWithFkConstraint = ({
  table,
  dataSourceName,
  relationship,
  suggestedRelationships,
}: {
  table: Table;
  dataSourceName: string;
  relationship: LocalTableArrayRelationship;
  suggestedRelationships: SuggestedRelationship[];
}): LocalRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType: 'Array',
    type: 'localRelationship',
    definition: getFkDefinition(table, relationship, suggestedRelationships),
  };
};

export const adaptRemoteSchemaRelationship = ({
  table,
  dataSourceName,
  relationship,
}: {
  table: Table;
  dataSourceName: string;
  relationship: SourceToRemoteSchemaRelationship;
}): RemoteSchemaRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType: 'Remote',
    type: 'remoteSchemaRelationship',
    definition: {
      toRemoteSchema: relationship.definition.to_remote_schema.remote_schema,
      lhs_fields: relationship.definition.to_remote_schema.lhs_fields,
      remote_field: relationship.definition.to_remote_schema.remote_field,
    },
  };
};

export const adaptLegacyRemoteSchemaRelationship = ({
  table,
  dataSourceName,
  relationship,
}: {
  table: Table;
  dataSourceName: string;
  relationship: Legacy_SourceToRemoteSchemaRelationship;
}): RemoteSchemaRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType: 'Remote',
    type: 'remoteSchemaRelationship',
    definition: {
      toRemoteSchema: relationship.definition.remote_schema,
      lhs_fields: relationship.definition.hasura_fields,
      remote_field: relationship.definition.remote_field,
    },
  };
};

export const adaptRemoteDatabaseRelationship = ({
  table,
  dataSourceName,
  relationship,
}: {
  table: Table;
  dataSourceName: string;
  relationship: SourceToSourceRelationship;
}): RemoteDatabaseRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType:
      relationship.definition.to_source.relationship_type === 'array'
        ? 'Array'
        : 'Object',
    type: 'remoteDatabaseRelationship',
    definition: {
      toSource: relationship.definition.to_source.source,
      toTable: relationship.definition.to_source.table,
      mapping: relationship.definition.to_source.field_mapping,
    },
  };
};
