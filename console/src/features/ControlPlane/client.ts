import endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { print, DocumentNode } from 'graphql/language';

export const controlPlaneClient = (
  endpoint: string = endpoints.luxDataGraphql
) => {
  const query = <
    ResponseType = Record<string, any>,
    VariablesType = Record<string, any>
  >(
    queryDoc: DocumentNode,
    variables: VariablesType,
    headers: Record<string, string> = {
      'content-type': 'application/json',
    }
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
