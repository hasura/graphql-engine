import { MetadataTable, Source } from '../../hasura-metadata-types';
import { formatSdl } from 'format-graphql';
import { getMutationRoot } from './getMutationRoot';

export const generateGraphQLInsertMutation = ({
  defaultQueryRoot,
  tableCustomization,
  sourceCustomization,
  objects,
  mutationName,
}: {
  mutationName?: string;
  defaultQueryRoot: string;
  tableCustomization?: MetadataTable['configuration'];
  sourceCustomization?: Source['customization'];
  objects: Record<string, string | number | boolean>[];
}) => {
  const queryRoot = getMutationRoot({
    defaultQueryRoot,
    operation: 'insert',
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

  const objectToInsert = objects
    .map(object => {
      return `{${Object.entries(object)
        .map(
          ([column, value]) =>
            `${column}: ${
              typeof value === 'string' ? JSON.stringify(value) : value
            }`
        )
        .join()}}`;
    })
    .join();

  /**
   * If the source has a GQL namespace set for it, then we query for our `queryRoot` under that namespace
   */
  if (sourceCustomization?.root_fields?.namespace) {
    const mutationString = `mutation ${mutationName ?? 'MyMutation'}  {
      ${sourceCustomization.root_fields.namespace} {
        ${queryRoot} (objects: [ ${objectToInsert} ]){
          affected_rows
        }
      }
    }`;

    return {
      query: formatSdl(mutationString),
      resultPath: `${sourceCustomization.root_fields?.namespace}.${queryRoot}`,
    };
  }

  const query = `mutation ${mutationName ?? 'MyMutation'} {
    ${queryRoot} (objects: [ ${objectToInsert} ]) {
      affected_rows
    }
  }`;

  return {
    query: formatSdl(query),
    resultPath: queryRoot,
  };
};
