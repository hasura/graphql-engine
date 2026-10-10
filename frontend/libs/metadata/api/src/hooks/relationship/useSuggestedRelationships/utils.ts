import { NamingConvention } from '@hasura/shared/types';
import { getTableDisplayName } from '@hasura/shared/utils';
import { SuggestedRelationship, SuggestedRelationshipWithName } from './types';
import { camelize, pluralize, singularize } from 'inflection';

export const addConstraintName = ({
  relationships,
  namingConvention,
}: {
  relationships: SuggestedRelationship[];
  namingConvention: NamingConvention | undefined;
}): SuggestedRelationshipWithName[] =>
  relationships.map((relationship) => {
    const fromTableName = getTableDisplayName(relationship.from.table);

    const toTableName =
      relationship.type === 'array'
        ? pluralize(getTableDisplayName(relationship.to.table))
        : singularize(getTableDisplayName(relationship.to.table));

    // make graphql compliant, replace "." with "_"
    const sanitizedTableName = toTableName.replace('.', '_');
    const constraintName =
      namingConvention === 'graphql-default'
        ? camelize(sanitizedTableName, true)
        : sanitizedTableName;

    const id = fromTableName + '_' + constraintName;

    return {
      ...relationship,
      constraintName,
      id,
    };
  });
