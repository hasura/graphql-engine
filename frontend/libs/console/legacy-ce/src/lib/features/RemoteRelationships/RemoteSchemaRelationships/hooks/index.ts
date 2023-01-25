import Endpoints from '@/Endpoints';
import { Api } from '@/hooks/apiUtils';
import { useAppSelector } from '@/store';
import { useMutation, useQueryClient } from 'react-query';

export const useAddRemoteSchemaRelationship = () => {
  const queryClient = useQueryClient();
  const headers = useAppSelector(state => state.tables.dataHeaders);

  return useMutation(
    (rel: any) =>
      Api.post<Record<string, unknown>>({
        headers,
        body: {
          type: 'pg_create_remote_relationship',
          args: rel,
        },
        url: Endpoints.metadata,
      }),
    {
      onSuccess: () => {
        queryClient.refetchQueries(['metadata'], { active: true });
      },
    }
  );
};
export const useDropRemoteSchemaRelationship = () => {
  const queryClient = useQueryClient();
  const headers = useAppSelector(state => state.tables.dataHeaders);

  return useMutation(
    (rel: any) =>
      Api.post<Record<string, string>>({
        headers,
        body: {
          type: 'pg_delete_remote_relationship',
          args: rel,
        },
        url: Endpoints.metadata,
      }),
    {
      onSuccess: () => {
        queryClient.refetchQueries(['metadata'], { active: true });
      },
      onError: () => {},
    }
  );
};

export const useUpdateRemoteSchemaRelationship = () => {
  const queryClient = useQueryClient();
  const headers = useAppSelector(state => state.tables.dataHeaders);

  return useMutation(
    (rel: any) =>
      Api.post<Record<string, string>>({
        headers,
        body: {
          type: 'pg_update_remote_relationship',
          args: rel,
        },
        url: Endpoints.metadata,
      }),
    {
      onSuccess: () => {
        queryClient.refetchQueries(['metadata'], { active: true });
      },
      onError: () => {},
    }
  );
};
