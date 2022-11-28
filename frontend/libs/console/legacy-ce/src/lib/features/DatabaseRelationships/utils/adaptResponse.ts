import {
  isSameTableObjectRelationship,
  TableFkRelationships,
} from '@/features/DataSource';
import {
  LocalTableArrayRelationship,
  LocalTableObjectRelationship,
  ManualArrayRelationship,
  ManualObjectRelationship,
  SameTableObjectRelationship,
  Table,
} from '@/features/hasura-metadata-types';
import { areTablesEqual } from '@/features/RelationshipsTable';
import isEqual from 'lodash.isequal';
import { LocalRelationship } from '../types';

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
  fkConstraints: TableFkRelationships[]
): { toTable: Table; mapping: Record<string, string> } => {
  if (isSameTableObjectRelationship(relationship)) {
    const fromTable = table;
    const fromColumns = Array.isArray(
      relationship.using.foreign_key_constraint_on
    )
      ? relationship.using.foreign_key_constraint_on
      : [relationship.using.foreign_key_constraint_on];

    const matchingFkConstraint = fkConstraints.find(
      fkConstraint =>
        areTablesEqual(fromTable, fkConstraint.from.table) &&
        isEqual(fromColumns.sort(), fkConstraint.from.column)
    );
    console.log(matchingFkConstraint, fkConstraints);
    // const

    return {
      toTable: matchingFkConstraint?.to.table,
      mapping: matchingFkConstraint
        ? getKeyValuePair(
            matchingFkConstraint.from.column,
            matchingFkConstraint.to.column
          )
        : {},
    };
  }

  const toTable = table;
  const toColumn =
    'column' in relationship.using.foreign_key_constraint_on
      ? [relationship.using.foreign_key_constraint_on.column]
      : relationship.using.foreign_key_constraint_on.columns;

  const matchingFkConstraint = fkConstraints.find(
    fkConstraint =>
      areTablesEqual(toTable, fkConstraint.to.table) &&
      isEqual(toColumn.sort(), fkConstraint.to.column)
  );
  console.log(matchingFkConstraint, fkConstraints);

  return {
    toTable: matchingFkConstraint?.from.table,
    mapping: matchingFkConstraint
      ? getKeyValuePair(
          matchingFkConstraint.to.column,
          matchingFkConstraint.from.column
        )
      : {},
  };
};

export const adaptLocalObjectRelationshipWithManualConfigruation = ({
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
  fkConstraints,
}: {
  table: Table;
  dataSourceName: string;
  relationship: SameTableObjectRelationship | LocalTableObjectRelationship;
  fkConstraints: TableFkRelationships[];
}): LocalRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType: 'Object',
    type: 'localRelationship',
    definition: getFkDefinition(table, relationship, fkConstraints),
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
  fkConstraints,
}: {
  table: Table;
  dataSourceName: string;
  relationship: LocalTableArrayRelationship;
  fkConstraints: TableFkRelationships[];
}): LocalRelationship => {
  return {
    name: relationship.name,
    fromSource: dataSourceName,
    fromTable: table,
    relationshipType: 'Array',
    type: 'localRelationship',
    definition: getFkDefinition(table, relationship, fkConstraints),
  };
};
