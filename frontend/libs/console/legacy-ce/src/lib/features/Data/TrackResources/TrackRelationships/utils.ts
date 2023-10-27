import isObject from 'lodash/isObject';
import { isSchemaTable } from '../../../DataSource/utils';
import { Table } from '../../../hasura-metadata-types';
import inflection from 'inflection';
import camelCase from 'lodash/camelCase';
import { NamingConvention } from '../../../hasura-metadata-types';
import { SuggestedRelationship, SuggestedRelationshipWithName } from './types';

/*
this function isn't entirely generic but it will hold for the current set of native DBs we have & GDC as well
*/
export const getTableDisplayName = (table: Table): string => {
  if (Array.isArray(table)) {
    return table.join('.');
  }

  if (!table) {
    return 'Empty Object';
  }

  if (typeof table === 'string') {
    return table;
  }

  if (typeof table === 'object' && isSchemaTable(table)) {
    return table.name;
  }

  if (isObject(table)) {
    const tableObj = table as Record<string, any>;
    return Object.keys(tableObj)
      .sort()
      .map(key => tableObj[key])
      .join('.');
  }

  return JSON.stringify(table);
};

export const addConstraintName = ({
  relationships,
  namingConvention,
}: {
  relationships: SuggestedRelationship[];
  namingConvention: NamingConvention | undefined;
}): SuggestedRelationshipWithName[] =>
  relationships.map(relationship => {
    const fromTableName = getTableDisplayName(relationship.from.table);

    const toTableName =
      relationship.type === 'array'
        ? inflection.pluralize(getTableDisplayName(relationship.to.table))
        : inflection.singularize(getTableDisplayName(relationship.to.table));

    // make graphql compliant, replace "." with "_"
    const sanitizedTableName = toTableName.replace('.', '_');
    const constraintName =
      namingConvention === 'graphql-default'
        ? camelCase(sanitizedTableName)
        : sanitizedTableName;

    const id = fromTableName + '_' + constraintName;

    return {
      ...relationship,
      constraintName,
      id,
    };
  });

/**
 *
 *
 * @param {string} query
 * @param {string[]} toSearch
 * @return a boolean indicating if the query was found in any of the string within toSearch
 */
export const anyIncludes = (query: string, toSearch: string[]) => {
  const lowered = query.toLowerCase();
  return toSearch.some(x => x.toLowerCase().includes(lowered));
};
