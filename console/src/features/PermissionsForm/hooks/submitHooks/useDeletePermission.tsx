import {
  useMetadataVersion,
  useMetadataMigration,
} from '@/features/MetadataAPI';
import { useAppSelector } from '@/store';
import { currentDriver } from '@/dataSources';

import { QueryType } from '../../types';
import { api } from '../../api';

export interface UseDeletePermissionArgs {
  tableName: string;
  schemaName: string;
  roleName: string;
}

const useDataTarget = () => {
  const dataSource: string = useAppSelector(
    state => state.tables.currentDataSource || 'default'
  );

  const driver = currentDriver;

  const { data: resourceVersion, isLoading, isError } = useMetadataVersion();

  if (!resourceVersion && !isLoading) {
    throw new Error('No resource version');
  }

  return {
    driver,
    dataSource,
    resourceVersion,
    isLoading,
    isError,
  };
};

export const useDeletePermission = ({
  tableName,
  schemaName,
  roleName,
}: UseDeletePermissionArgs) => {
  const {
    driver,
    dataSource,
    resourceVersion,
    isLoading: dataTargetLoading,
    isError: dataTargetError,
  } = useDataTarget();
  const mutate = useMetadataMigration();

  const submit = async (queries: QueryType[]) => {
    if (!resourceVersion) {
      console.error('No resource version');
      return;
    }

    const body = api.createDeleteBody({
      driver,
      dataTarget: {
        database: dataSource,
        table: tableName,
        schema: schemaName,
      },
      roleName,
      resourceVersion,
      queries,
    });

    await mutate.mutateAsync({
      source: dataSource,
      query: body,
      migrationName: 'deletePermission',
    });
  };

  const isLoading = mutate.isLoading || dataTargetLoading;
  const isError = mutate.isError || dataTargetError;

  return {
    submit,
    ...mutate,
    isLoading,
    isError,
  };
};
