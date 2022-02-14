import { useMutation, useQueryClient } from 'react-query';

import { useAppSelector } from '@/store';
import { useMetadataVersion } from '../../../MetadataAPI';

import { AccessType, FormOutput, QueryType } from '../../types';
import { api, cache } from '../../api';

export interface UseSubmitFormArgs {
  tableName: string;
  schemaName: string;
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

export const useSubmitForm = (args: UseSubmitFormArgs) => {
  const client = useQueryClient();
  const headers = useAppSelector(state => state.tables.dataHeaders);
  const { data: resourceVersion } = useMetadataVersion();
  const { handleUpdate } = cache.useUpdateTablePermissionCache();

  const { mutateAsync, ...rest } = useMutation(api.createPermissions, {
    mutationKey: 'permissionsUpdate',
    // this performs an optimistic cache update
    // once the mutation has resolved the cache will be updated if it failed
    onMutate: response => handleUpdate({ args, response }),
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

  const submit = async (formData: FormOutput) => {
    if (!resourceVersion) {
      throw new Error('No resource version provided');
    }
    const { tableName, schemaName, roleName, queryType, accessType } = args;

    const body = api.createInsertBody({
      dataSource: 'pg',
      qualifiedTable: { name: tableName, schema: schemaName },
      roleName,
      queryType,
      accessType,
      resourceVersion,
      formData,
    });
    await mutateAsync({ headers, body });
  };

  return { submit, ...rest };
};
