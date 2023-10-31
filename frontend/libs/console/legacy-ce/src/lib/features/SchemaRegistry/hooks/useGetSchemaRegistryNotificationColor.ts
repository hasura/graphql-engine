import { useQuery } from 'react-query';
import { FETCH_SCHEMA_REGISTRY_NOTIFICATION_QUERY_NAME } from '../constants';
import { FETCH_SCHEMA_REGISTRY_NOTIFICATION_QUERY } from '../queries';
import { GetSchemaRegistryNotificationResponseWithError } from '../types';
import { schemaRegsitryControlPlaneClient } from '../utils';

type FetchSchemaRegistryNotificationResponse =
  | {
      kind: 'loading';
    }
  | {
      kind: 'error';
    }
  | {
      kind: 'success';
      response: NonNullable<
        GetSchemaRegistryNotificationResponseWithError['data']
      >;
    };

export const useGetSchemaRegistryNotificationColor = (
  projectId: string
): FetchSchemaRegistryNotificationResponse => {
  const fetchSchemaRegistyNotificationFn = (projectId: string) => {
    return schemaRegsitryControlPlaneClient.query<
      GetSchemaRegistryNotificationResponseWithError,
      { projectId: string }
    >(FETCH_SCHEMA_REGISTRY_NOTIFICATION_QUERY, {
      projectId: projectId,
    });
  };
  const { data, error, isLoading } = useQuery({
    queryKey: FETCH_SCHEMA_REGISTRY_NOTIFICATION_QUERY_NAME,
    queryFn: () => fetchSchemaRegistyNotificationFn(projectId),
    refetchOnMount: 'always',
    refetchOnWindowFocus: true,
  });
  if (isLoading) {
    return {
      kind: 'loading',
    };
  }
  if (error || !data || !!data?.errors || !data?.data) {
    return {
      kind: 'error',
    };
  }
  return {
    kind: 'success',
    response: data.data,
  };
};
