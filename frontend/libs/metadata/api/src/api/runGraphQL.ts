import { GraphQLRequestInput } from '@hasura/shared/types';
import { NetworkArgs } from '../types';
import { GraphQLError } from 'graphql';

export async function runGraphQL<T = any>({
  url,
  fetchJson,
  headers,
  body,
}: NetworkArgs & {
  headers?: Record<string, string>;
  body: GraphQLRequestInput;
}): Promise<T> {
  const result = await fetchJson(url, {
    headers,
    method: 'POST',
    body: JSON.stringify(body),
  });

  // Throw the first GraphQL Error
  // We do this because response.status is 200 even if there are errors
  if (result?.errors?.length) {
    const [error] = result.errors;
    throw new GraphQLError(error.message, { extensions: error.extensions });
  }

  return result.data;
}
