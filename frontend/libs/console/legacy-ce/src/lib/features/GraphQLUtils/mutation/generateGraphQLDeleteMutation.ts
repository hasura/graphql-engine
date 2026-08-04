import { formatSdl } from 'format-graphql';
import { MetadataTable, Source } from '../../hasura-metadata-types';
import { getMutationRoot } from './getMutationRoot';

export const generateGraphQLDeleteMutation = ({
  defaultQueryRoot,
  tableCustomization,
  sourceCustomization,
  rows,
  mutationName,
}: {
  mutationName: string;
  defaultQueryRoot: string;
  tableCustomization?: MetadataTable['configuration'];
  sourceCustomization?: Source['customization'];
  rows: Record<string, string | number | boolean>[];
}) => {
  const queryRoot = getMutationRoot({
    defaultQueryRoot,
    operation: 'delete',
    /**
     * Configuration contains the custom names for the following -
     * 1. Table Name
     * 2. Query roots - select, select_by_pk, select_aggregate
     * 3. Column Names
     *
     * Custom names from metadata are user-provided values and will always assume priority in the final GQL schema
     */
    configuration: tableCustomization,
    sourceCustomization,
  });

  const whereClauses = rows
    .map(row => {
      return `{ ${Object.entries(row).map(([fieldName, value]) => {
        return `${fieldName}: {_eq: ${
          typeof value === 'string' ? `"${value}"` : value
        }}`;
      })} }`;
    })
    .join(',');

  /**
   * If the source has a GQL namespace set for it, then we query for our `queryRoot` under that namespace
   */
  if (sourceCustomization?.root_fields?.namespace)
    return {
      query: formatSdl(`mutation ${mutationName}  {
    ${sourceCustomization.root_fields.namespace}  {
      ${queryRoot} (where: { 
        _or: [
        ${whereClauses}
      ] }) {
        affected_rows
      }
    }
  }`),
      resultPath: `${sourceCustomization.root_fields?.namespace}.${queryRoot}`,
    };

  return {
    query: formatSdl(`
    mutation ${mutationName} {
      ${queryRoot} (where: { 
        _or: [
        ${whereClauses}
        ] 
      }) {
        affected_rows
      }
    }
  `),
    resultPath: queryRoot,
  };
};
