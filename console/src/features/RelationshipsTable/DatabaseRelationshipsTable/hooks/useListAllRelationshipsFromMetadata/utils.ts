import {
  TableFkRelationships,
  isLegacyFkConstraint,
} from '@/features/DataSource';
import {
  Legacy_SourceToRemoteSchemaRelationship,
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SourceToSourceRelationship,
  SourceToRemoteSchemaRelationship,
  SameTableObjectRelationship,
  Table,
} from '@/features/MetadataAPI';
import { getRemoteFieldPath } from '@/features/RelationshipsTable';
import { Relationship } from '../../types';

export const getTargetColumns = (
  tableRlns: TableFkRelationships[],
  fkConstraint: string[]
) => {
  const destination = tableRlns.find(tr => {
    return tr.from.column.find(c =>
      fkConstraint.includes(c?.replace(/"/g, ''))
    );
  });
  return destination?.to.column ?? [];
};

export const adaptRemoteSchemaRelationship = (
  dataSourceName: string,
  table: Table,
  relationship: SourceToRemoteSchemaRelationship
): Relationship & { type: 'toRemoteSchema' } => {
  return {
    name: relationship.name,
    type: 'toRemoteSchema',
    toRemoteSchema: relationship.definition.to_remote_schema.remote_schema,
    relationship_type: 'Remote Schema',
    mapping: {
      from: {
        source: dataSourceName,
        table,
        columns: relationship.definition.to_remote_schema.lhs_fields,
      },
      to: {
        remoteSchema: relationship.definition.to_remote_schema.remote_schema,
        fields: getRemoteFieldPath(
          relationship.definition.to_remote_schema.remote_field
        ),
      },
    },
  };
};

export const adaptLegacyRemoteSchemaRelationship = (
  dataSourceName: string,
  table: Table,
  relationship: Legacy_SourceToRemoteSchemaRelationship
): Relationship & { type: 'toRemoteSchema' } => {
  return {
    name: relationship.name,
    type: 'toRemoteSchema',
    toRemoteSchema: relationship.definition.remote_schema,
    relationship_type: 'Remote Schema',
    mapping: {
      from: {
        source: dataSourceName,
        table,
        columns: relationship.definition.hasura_fields,
      },
      to: {
        remoteSchema: relationship.definition.remote_schema,
        fields: getRemoteFieldPath(relationship.definition.remote_field),
      },
    },
  };
};

export const adaptRemoteDBRelationship = (
  dataSourceName: string,
  table: Table,
  relationship: SourceToSourceRelationship
): Relationship & { type: 'toSource' } => {
  return {
    name: relationship.name,
    type: 'toSource',
    toSource: relationship.definition.to_source.source,
    relationship_type: relationship.definition.to_source.relationship_type,
    mapping: {
      from: {
        source: dataSourceName,
        table,
        columns: Object.keys(relationship.definition.to_source.field_mapping),
      },
      to: {
        source: relationship.definition.to_source.source,
        table: relationship.definition.to_source.table,
        columns: Object.values(relationship.definition.to_source.field_mapping),
      },
    },
  };
};

export const adaptManualRelationship = (
  dataSourceName: string,
  table: Table,
  relationship: ManualObjectRelationship | ManualArrayRelationship
): Relationship & { type: 'toLocalTableManual' } => {
  return {
    name: relationship.name,
    type: 'toLocalTableManual',
    toLocalTable: table,
    relationship_type: 'Object',
    mapping: {
      from: {
        source: dataSourceName,
        table,
        columns: Object.keys(
          relationship.using.manual_configuration.column_mapping
        ),
      },
      to: {
        source: dataSourceName,
        table: relationship.using.manual_configuration.remote_table,
        columns: Object.values(
          relationship.using.manual_configuration.column_mapping
        ),
      },
    },
  };
};

export const adaptLocalTableRelationship = (
  dataSourceName: string,
  table: Table,
  relationship: LocalTableObjectRelationship | LocalTableArrayRelationship,
  fkRelationships: TableFkRelationships[]
): Relationship & { type: 'toLocalTableFk' } => {
  const columns = isLegacyFkConstraint(
    relationship.using.foreign_key_constraint_on
  )
    ? [relationship.using.foreign_key_constraint_on.column]
    : relationship.using.foreign_key_constraint_on.columns;

  return {
    name: relationship.name,
    type: 'toLocalTableFk',
    toLocalTable: table,
    relationship_type: 'Object',
    mapping: {
      from: {
        source: dataSourceName,
        table,
        columns,
      },
      to: {
        source: dataSourceName,
        table: relationship.using.foreign_key_constraint_on.table,
        columns: getTargetColumns(fkRelationships, columns),
      },
    },
  };
};

export const adaptSameTableObjectRelationship = (
  dataSourceName: string,
  table: Table,
  relationship: SameTableObjectRelationship,
  fkRelationships: TableFkRelationships[]
): Relationship & { type: 'toSameTableFk' } => {
  const columns =
    typeof relationship.using.foreign_key_constraint_on === 'string'
      ? [relationship.using.foreign_key_constraint_on]
      : relationship.using.foreign_key_constraint_on;

  return {
    name: relationship.name,
    type: 'toSameTableFk',
    toLocalTable: table,
    relationship_type: 'Object',
    mapping: {
      from: {
        source: dataSourceName,
        table,
        columns,
      },
      to: {
        source: dataSourceName,
        table,
        columns: getTargetColumns(fkRelationships, columns),
      },
    },
  };
};
