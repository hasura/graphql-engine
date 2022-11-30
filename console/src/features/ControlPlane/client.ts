import endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { print, DocumentNode } from 'graphql/language';

export const controlPlaneClient = (
  endpoint: string = endpoints.luxDataGraphql,
  headers = {
    'content-type': 'application/json',
    'hasura-client-name': 'hasura-console',
  }
) => {
  const query = <
    ResponseType = Record<string, any>,
    VariablesType = Record<string, any>
  >(
    queryDoc: DocumentNode,
    variables: VariablesType
  ): Promise<ResponseType> => {
    return Api.post<ResponseType>({
      url: endpoint,
      headers,
      body: {
        query: print(queryDoc),
        variables: variables || {},
      },
      credentials: 'include',
    });
  };
  return {
    query,
  };
};
