import inflection from 'inflection';
import camelCase from 'lodash/camelCase';
import { SuggestedRelationship } from '../../../types';
import { getTableDisplayName } from '../../../utils/helpers';
import { NamingConvention, Table } from '../../../../hasura-metadata-types';
import { areTablesEqual } from '../../../../hasura-metadata-api';

export type SuggestedRelationshipsResponse = {
  relationships: SuggestedRelationship[];
};

type FilterTableRelationshipsArgs = {
  table: Table;
  relationships: SuggestedRelationship[];
};

export const filterTableRelationships = ({
  table,
  relationships,
}: FilterTableRelationshipsArgs) =>
  relationships.filter(relationship => {
    if (areTablesEqual(relationship.from.table, relationship.to.table)) {
      return false;
    }
    return areTablesEqual(relationship.from.table, table);
  });

export type SuggestedRelationshipWithName = SuggestedRelationship & {
  constraintName: string;
};

type GetRelationTableNameArg = {
  table: Table;
  relationshipType: SuggestedRelationship['type'];
};

const formatRelationToTableName = ({
  table,
  relationshipType,
}: GetRelationTableNameArg) => {
  const baseTableName = getTableDisplayName(table);
  if (relationshipType === 'array') {
    return inflection.pluralize(baseTableName);
  }

  return inflection.singularize(getTableDisplayName(table));
};

const makeStringGraphQLCompliant = (text: string) => text.replace(/\./g, '_');

export const addConstraintName = (
  relationships: SuggestedRelationship[],
  namingConvention: NamingConvention
): SuggestedRelationshipWithName[] =>
  relationships.map(relationship => {
    const toTableName = formatRelationToTableName({
      table: relationship.to.table,
      relationshipType: relationship.type,
    });

    const baseConstraintName = makeStringGraphQLCompliant(toTableName);

    const constraintName =
      namingConvention === 'graphql-default'
        ? camelCase(baseConstraintName)
        : baseConstraintName;

    return {
      ...relationship,
      constraintName,
    };
  });
