import { useQueryClient } from '@tanstack/react-query';
import {
  type DataQueryType,
  type Metadata,
  type Table,
  type AccessType,
  keyToPermission,
} from '@hasura/shared/types';
import { PermissionsSchema } from '../../../schema';
import { isPermission } from '../../../utils';
import { api } from '../../api';
import { permissionsTableKey } from '../../../PermissionsTable/hooks';
import { hasuraToast } from '@hasura/shared/ui';
import {
  MetadataMigrationOptions,
  useErrorNotification,
  useMetadataHelpers,
  useMetadataMigration,
} from '@hasura/metadata/api';
import type { TablePermissionInputValidationSchema } from '../../components/InputValidation/InputValidation';

export interface UseSubmitFormArgs {
  dataSourceName: string;
  table: Table;
  roleName: string;
  queryType: DataQueryType;
  accessType: AccessType;
  validateInput?: TablePermissionInputValidationSchema;
}

interface ExistingPermissions {
  role: string;
  queryType: DataQueryType;
  table: Table;
}

const getAllPermissions = async (
  dataSourceName: string,
  metadata: Metadata['metadata'],
) => {
  // find current source
  const currentMetadataSource = metadata.sources?.find(
    (source) => source.name === dataSourceName,
  );

  return currentMetadataSource?.tables.reduce<ExistingPermissions[]>(
    (acc, metadataTable) => {
      Object.entries(metadataTable).forEach(([key, value]) => {
        const props = { key, value };
        if (isPermission(props)) {
          props.value.forEach((permission) => {
            acc.push({
              role: permission.role,
              queryType: keyToPermission[props.key],
              table: metadataTable.table,
            });
          });
        }
      });

      return acc;
    },
    [],
  );
};

export const useSubmitForm = (args: UseSubmitFormArgs) => {
  const { dataSourceName, table, roleName, queryType, accessType } = args;

  const queryClient = useQueryClient();
  const { mutate, ...mutation } = useMetadataMigration();
  const { fetchMetadata } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const submit = async (
    formData: PermissionsSchema,
    options?: MetadataMigrationOptions,
  ) => {
    const data = await fetchMetadata();

    const metadataSource = data?.metadata?.sources.find(
      (s) => s.name === dataSourceName,
    );

    if (!data?.resource_version || !metadataSource) {
      console.error('Something went wrong!');
      return;
    }

    const existingPermissions = await getAllPermissions(
      dataSourceName,
      data.metadata,
    );

    const body = api.createInsertBody({
      dataSourceName,
      driver: metadataSource.kind,
      table,
      role: roleName,
      queryType,
      accessType,
      resourceVersion: data.resource_version,
      formData,
      existingPermissions: existingPermissions ?? [],
    });

    mutate(
      {
        query: body,
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          queryClient.invalidateQueries({
            queryKey: permissionsTableKey({
              dataSourceName,
              table,
            }),
          });

          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Permissions saved successfully!',
          });
          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (err, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Error!',
            error: err,
          });
          options?.onError?.(err, variables, onMutateResult, context);
        },
      },
    );
  };

  return {
    submit,
    ...mutation,
  };
};
