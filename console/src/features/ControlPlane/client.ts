import globals from '@/Globals';
import endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { print, DocumentNode } from 'graphql/language';

export const makeClient = (
  g: typeof globals,
  endpoint: string,
  headers = {}
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

export const client = makeClient(globals, endpoints.luxDataGraphql, {
  'content-type': 'application/json',
});
