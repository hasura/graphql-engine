import { getGraphQLQueryPayload } from '../../../../../Common/utils/graphqlUtils';
import Endpoints from '../../../../../../Endpoints';
import { Api } from '../../../../../../hooks/apiUtils';
import { useAppSelector } from '../../../../../../storeHooks';
import { getIntrospectionQuery, IntrospectionQuery } from 'graphql';
import { useQuery, UseQueryOptions, UseQueryResult } from 'react-query';

type IntrospectionQueryResp = {
  data: IntrospectionQuery;
};

export function useIntrospectionSchema(): UseQueryResult<
  IntrospectionQueryResp,
  Error
>;
export function useIntrospectionSchema<
  T extends (d: IntrospectionQueryResp) => any
>(select: T): UseQueryResult<ReturnType<T>, Error>;

export function useIntrospectionSchema(
  select = (d: IntrospectionQueryResp) => d,
  transformFn = (d: unknown) => d,
  queryOptions?: Omit<
    UseQueryOptions<
      IntrospectionQueryResp,
      Error,
      unknown,
      'introspectionSchema'
    >,
    'queryKey' | 'queryFn'
  >
) {
  const headers = useAppSelector(state => state.tables.dataHeaders);
  const queryFn = () => {
    return Api.post<IntrospectionQueryResp>({
      headers,
      body: getGraphQLQueryPayload(getIntrospectionQuery(), {}),
      url: Endpoints.graphQLUrl,
    });
  };

  return useQuery({
    queryKey: 'introspectionSchema',
    queryFn,
    ...queryOptions,
    select: d => transformFn(select(d)),
  });
}
