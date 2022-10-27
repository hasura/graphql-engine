import React from 'react';
import { buildClientSchema, GraphQLSchema, IntrospectionQuery } from 'graphql';
import { useHttpClient } from '@/features/Network';
import { runIntrospectionQuery } from '@/features/DataSource';

import { createDefaultValues, getAllColumnsAndOperators } from '../utils';

/**
 *
 * fetch the schema from the gql server
 */
export const useIntrospectSchema = () => {
  const httpClient = useHttpClient();

  const [schema, setSchema] = React.useState<GraphQLSchema>();

  const getSchema = React.useCallback(async () => {
    const introspectionResult: { data: IntrospectionQuery } =
      await runIntrospectionQuery({ httpClient });
    const result = buildClientSchema(introspectionResult.data);
    setSchema(result);
  }, []);

  React.useEffect(() => {
    getSchema();
  }, [getSchema]);

  return { data: schema };
};

interface Args {
  tableName: string;
  schema?: GraphQLSchema;
}

/**
 *
 * get all boolOperators, columns and relationships
 * and information about types for each
 */
export const useData = ({ tableName, schema }: Args) => {
  if (!schema)
    return {
      data: {
        boolOperators: [],
        columns: [],
        relationships: [],
      },
    };
  const data = getAllColumnsAndOperators({ tableName, schema });
  return { data };
};

interface A {
  tableName: string;
  existingPermission: Record<string, any>;
}

export const useCreateRowPermissionsDefaults = () => {
  const { data: schema } = useIntrospectSchema();

  const fetchDefaults = async ({ tableName, existingPermission }: A) => {
    createDefaultValues({ tableName, schema, existingPermission });
  };

  return fetchDefaults;
};
