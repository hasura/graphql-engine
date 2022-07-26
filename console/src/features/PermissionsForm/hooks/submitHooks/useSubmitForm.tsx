import {
  useMetadataMigration,
  useMetadataPermissions,
  useMetadataVersion,
} from '@/features/MetadataAPI';

import { AccessType, FormOutput, QueryType } from '../../types';
import { api } from '../../api';
import { NewDataTarget } from '../../../PermissionsTab/types/types';

export interface UseSubmitFormArgs {
  dataTarget: NewDataTarget;
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
}

export const useSubmitForm = (args: UseSubmitFormArgs) => {
  const { dataTarget, roleName, queryType, accessType } = args;
  const {
    data: resourceVersion,
    isLoading: resourceVersionLoading,
    isError: resourceVersionError,
  } = useMetadataVersion();

  const mutate = useMetadataMigration();

  const {
    data: existingPermissions,
    isLoading: existingPermissionsLoading,
    isError: existingPermissionsError,
  } = useMetadataPermissions(args.dataTarget.dataSource.database);

  const submit = async (formData: FormOutput) => {
    if (!resourceVersion) {
      console.error('No resource version');
      return;
    }

    const body = api.createInsertBody({
      dataTarget,
      roleName,
      queryType,
      accessType,
      resourceVersion,
      formData,
      existingPermissions,
    });

    await mutate.mutateAsync({
      query: body,
    });
  };

  const isLoading =
    mutate.isLoading || resourceVersionLoading || existingPermissionsLoading;
  const isError =
    mutate.isError || resourceVersionError || existingPermissionsError;

  return {
    submit,
    ...mutate,
    isLoading,
    isError,
  };
};
