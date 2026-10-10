import {
  ADMIN_SECRET_HEADER_KEY,
  GraphQLRequestInput,
} from '@hasura/shared/types';
import endpoints from '../../../../Endpoints';
import { requestJson } from '@hasura/shared/utils';
import { showErrorNotification } from '@hasura/shared/ui';

export type ExplainResult = {
  field?: string;
  sql: string;
  plan: string[];
};

export const analyzeFetcher = (
  query: GraphQLRequestInput,
  headers: Record<string, string>,
  isRelay: boolean,
): Promise<ExplainResult[]> => {
  const editedQuery = {
    query,
    is_relay: isRelay,
    user: {
      'x-hasura-role': 'admin',
    },
  };

  // Check if x-hasura-role is available in some form in the headers
  const reqHeaders = Object.entries(headers).reduce(
    (acc, [key, value]) => {
      // If header has x-hasura-*
      const lHead = key.toLowerCase();
      if (
        lHead.slice(0, 'x-hasura-'.length) === 'x-hasura-' &&
        lHead !== ADMIN_SECRET_HEADER_KEY
      ) {
        editedQuery.user[lHead] = value;

        return acc;
      }

      acc[key] = value;

      return acc;
    },
    {} as Record<string, any>,
  );

  return requestJson<ExplainResult[]>(`${endpoints.graphQLUrl}/explain`, {
    method: 'POST',
    headers: reqHeaders,
    body: JSON.stringify(editedQuery),
    credentials: 'include',
  })
    .then((result) => (Array.isArray(result) ? result : [result]))
    .catch((err) => {
      showErrorNotification({
        title: 'Analyzing query error',
        error: err,
      });

      return [];
    });
};
