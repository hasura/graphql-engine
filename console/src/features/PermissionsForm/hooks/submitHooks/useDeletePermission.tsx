import { useQueryClient } from 'react-query';
import { useMetadataMigration } from '@/features/MetadataAPI';
import { exportMetadata } from '@/features/DataSource';

import { useHttpClient } from '@/features/Network';

import { QueryType } from '../../types';
import { api } from '../../api';

export interface UseDeletePermissionArgs {
  currentSource: string;
  dataSourceName: string;
  table: unknown;
  roleName: string;
}

export const useDeletePermission = ({
  currentSource,
  dataSourceName,
  table,
  roleName,
}: UseDeletePermissionArgs) => {
  const mutate = useMetadataMigration();
  const httpClient = useHttpClient();
  const queryClient = useQueryClient();

  const submit = async (queries: QueryType[]) => {
    const { resource_version: resourceVersion } = await exportMetadata({
      httpClient,
    });

    if (!resourceVersion) {
      console.error('No resource version');
      return;
    }

    const body = api.createDeleteBody({
      currentSource,
      dataSourceName,
      table,
      roleName,
      resourceVersion,
      queries,
    });

    await mutate.mutateAsync({
      query: body,
    });

    queryClient.invalidateQueries([dataSourceName, 'permissionsTable']);
  };

  const isLoading = mutate.isLoading;
  const isError = mutate.isError;

  return {
    submit,
    ...mutate,
    isLoading,
    isError,
  };
};
