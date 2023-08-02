import { useMutation } from 'react-query';
import Endpoints from '../../../../Endpoints';
import { Api } from '../../../../hooks/apiUtils';
import { useAppSelector } from '../../../../storeHooks';
import { useInvalidateMetadata } from '../../../hasura-metadata-api';

export const useAddRemoteSchemaRelationship = () => {
  const invalidateMetadata = useInvalidateMetadata();
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
        invalidateMetadata({
          componentName: 'useAddRemoteSchemaRelationship',
          reasons: ['added a remote schema relationship'],
        });
      },
    }
  );
};
export const useDropRemoteSchemaRelationship = () => {
  const invalidateMetadata = useInvalidateMetadata();
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
        invalidateMetadata({
          componentName: 'useDropRemoteSchemaRelationship',
          reasons: ['dropped a remote schema relationship'],
        });
      },
      onError: () => {},
    }
  );
};

export const useUpdateRemoteSchemaRelationship = () => {
  const invalidateMetadata = useInvalidateMetadata();
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
        invalidateMetadata({
          componentName: 'useUpdateRemoteSchemaRelationship',
          reasons: ['updated a remote schema relationship'],
        });
      },
      onError: () => {},
    }
  );
};
