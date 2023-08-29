import { AxiosInstance } from 'axios';
import { useQueryClient } from 'react-query';
import { exportMetadata } from '../../../../DataSource';
import { useMetadataMigration } from '../../../../MetadataAPI';
import { useHttpClient } from '../../../../Network';
import { Table } from '../../../../hasura-metadata-types';
import { PermissionsSchema } from '../../../schema';
import { AccessType, QueryType } from '../../../types';
import { isPermission, keyToPermission } from '../../../utils';
import { api } from '../../api';
import { permissionsTableKey } from '../../../PermissionsTable/hooks';
import { permissionsFormKey } from '../dataFetchingHooks';
import { transformErrorResponse } from '../../../../Data/errorUtils';
import { hasuraToast } from '../../../../../new-components/Toasts';
import { DisplayToastErrorMessage } from '../../../../Data/components/DisplayErrorMessage';
import { inputValidationSchema } from '../../../../../components/Services/Data/TablePermissions/InputValidation/InputValidation';
import { z } from 'zod';

export interface UseSubmitFormArgs {
  dataSourceName: string;
  table: Table;
  tables: Table[];
  roleName: string;
  queryType: QueryType;
  accessType: AccessType;
  validateInput?: z.infer<typeof inputValidationSchema>;
}

interface ExistingPermissions {
  role: string;
  queryType: QueryType;
  table: Table;
}

interface GetAllPermissionsArgs {
  dataSourceName: string;
  httpClient: AxiosInstance;
}

const getAllPermissions = async ({
  dataSourceName,
  httpClient,
}: GetAllPermissionsArgs) => {
  const { metadata } = await exportMetadata({ httpClient });

  // find current source
  const currentMetadataSource = metadata?.sources?.find(
    source => source.name === dataSourceName
  );

  return currentMetadataSource?.tables.reduce<ExistingPermissions[]>(
    (acc, metadataTable) => {
      Object.entries(metadataTable).forEach(([key, value]) => {
        const props = { key, value };
        if (isPermission(props)) {
          props.value.forEach(permission => {
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
    []
  );
};

export const useSubmitForm = (args: UseSubmitFormArgs) => {
  const { dataSourceName, table, roleName, queryType, accessType, tables } =
    args;

  const queryClient = useQueryClient();

  const httpClient = useHttpClient();

  const mutate = useMetadataMigration(
    {
      errorTransform: transformErrorResponse,
    },
    ['roles']
  );

  const submit = async (formData: PermissionsSchema) => {
    const { metadata, resource_version } = await exportMetadata({
      httpClient,
    });

    const metadataSource = metadata?.sources.find(
      s => s.name === dataSourceName
    );

    if (!resource_version || !metadataSource) {
      console.error('Something went wrong!');
      return;
    }

    const existingPermissions = await getAllPermissions({
      dataSourceName,
      httpClient,
    });

    const body = api.createInsertBody({
      dataSourceName,
      driver: metadataSource.kind,
      table,
      tables,
      role: roleName,
      queryType,
      accessType,
      resourceVersion: resource_version,
      formData,
      existingPermissions: existingPermissions ?? [],
    });
    await mutate.mutateAsync(
      {
        query: body,
      },
      {
        onSuccess: () => {
          hasuraToast({
            type: 'success',
            title: 'Success!',
            message: 'Permissions saved successfully!',
          });
          exportMetadata({
            httpClient,
          });
        },
        onError: err => {
          hasuraToast({
            type: 'error',
            title: 'Error!',
            children: <DisplayToastErrorMessage message={err.message} />,
          });
        },
        onSettled: () => {
          queryClient.invalidateQueries(
            permissionsFormKey({
              dataSourceName,
              table,
            })
          );
          queryClient.invalidateQueries(
            permissionsTableKey({
              dataSourceName,
              table,
            })
          );
        },
      }
    );
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
