import { useAppSelector } from '@/store';
import { currentDriver } from '@/dataSources';
import {
  useMetadataMigration,
  useMetadataVersion,
} from '@/features/MetadataAPI';

import { AccessType, FormOutput, QueryType } from '../../types';
import { api } from '../../api';

export interface UseSubmitFormArgs {
  tableName: string;
  schemaName: string;
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
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

export const useSubmitForm = (args: UseSubmitFormArgs) => {
  const {
    driver,
    dataSource,
    resourceVersion,
    isLoading: dataTargetLoading,
    isError: dataTargetError,
  } = useDataTarget();

  const mutate = useMetadataMigration();

  const submit = async (formData: FormOutput) => {
    if (!resourceVersion) {
      console.error('No resource version');
      return;
    }
    const { tableName, schemaName, roleName, queryType, accessType } = args;

    const body = api.createInsertBody({
      driver,
      dataTarget: {
        database: dataSource,
        table: tableName,
        schema: schemaName,
      },
      roleName,
      queryType,
      accessType,
      resourceVersion,
      formData,
    });

    await mutate.mutateAsync({
      source: dataSource,
      query: body,
      migrationName: 'permissionsUpdate',
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
