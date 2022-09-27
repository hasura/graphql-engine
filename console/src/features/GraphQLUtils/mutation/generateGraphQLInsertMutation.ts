import { MetadataTable, Source } from '@/features/MetadataAPI';
import { getMutationRoot } from './getMutationRoot';

export const generateGraphQLMutation = async ({
  defaultQueryRoot,
  tableCustomization,
  sourceCustomization,
  objects,
  operationName,
}: {
  operationName?: string;
  defaultQueryRoot: string;
  tableCustomization: MetadataTable['configuration'];
  sourceCustomization: Source['customization'];
  objects: Record<string, string | number | boolean>;
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

  const objectToInsert = Object.entries(objects).reduce<string>((acc, x) => {
    const [column, value] = x;
    return `${column}: ${typeof value === 'string' ? `"${value}"` : value}`;
  }, '');

  /**
   * If the source has a GQL namespace set for it, then we query for our `queryRoot` under that namespace
   */
  if (sourceCustomization?.root_fields?.namespace)
    return {
      query: `mutation ${operationName ?? 'MyMutation'}  {
    ${
      sourceCustomization.root_fields.namespace
    } (objects: { ${objectToInsert} }) {
      ${queryRoot} {
        affected_rows
      }
    }
  }`,
      resultPath: `${sourceCustomization.root_fields?.namespace}.${queryRoot}`,
    };

  return {
    query: `query ${operationName ?? 'MyQuery'} {
    ${queryRoot} (objects: { ${objectToInsert} }) {
      affected_rows
    }
  }`,
    resultPath: queryRoot,
  };
};
