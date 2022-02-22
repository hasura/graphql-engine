import { useMutation, useQueryClient } from 'react-query';

import { useMetadataVersion } from '@/features/MetadataAPI';
import { useAppSelector } from '@/store';

import { QueryType } from '../../types';
import { api } from '../../api';

export interface UseDeletePermissionArgs {
  tableName: string;
  schemaName: string;
  roleName: string;
}

export const useDeletePermission = ({
  tableName,
  schemaName,
  roleName,
}: UseDeletePermissionArgs) => {
  const client = useQueryClient();

  const headers = useAppSelector(state => state.tables.dataHeaders);
  const { data: resourceVersion } = useMetadataVersion();

  const { mutateAsync, ...rest } = useMutation(api.deletePermissions, {
    onError: (_err, _, context: any) => {
      // if there is an error set the metadata query to the original value
      client.setQueryData('metadata', context.metadata);
    },

    onSettled: () => {
      // once the mutation is complete invalidate the query
      // ensure the client state is upto date with the server state
      client.invalidateQueries('metadata');
    },
  });

  const submit = async (queries: QueryType[]) => {
    if (!resourceVersion) {
      throw new Error('No resource version provided');
    }

    const body = api.createDeleteBody({
      dataSource: 'pg',
      qualifiedTable: { name: tableName, schema: schemaName },
      roleName,
      resourceVersion,
      queries,
    });

    await mutateAsync({ headers, body });
  };

  return { submit, ...rest };
};
