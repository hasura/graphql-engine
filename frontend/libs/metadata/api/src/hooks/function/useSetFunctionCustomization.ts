import { Table, TableFunction } from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { hasuraToast } from '@hasura/shared/ui';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';
import { useErrorNotification } from '../notification';

type Configuration = {
  custom_root_fields?: {
    function_aggregate?: string;
    function?: string;
  };
  custom_name?: string;
  response?: {
    table: Table;
    type: string;
  };
};

type SetFunctionCustomizationArgs = {
  dataSourceName: string;
  // the plain function name is used by native databases, the qualified function by GDC data sources
  func: TableFunction;
  configuration: Configuration;
};

export const useSetFunctionCustomization = () => {
  const { mutate, ...mutation } = useMetadataMigration();
  const { fetchSource } = useMetadataHelpers();
  const showErrorNotification = useErrorNotification();

  const setFunctionCustomization = async (
    { dataSourceName, func, configuration }: SetFunctionCustomizationArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const { source, resource_version } = await fetchSource(dataSourceName);

    const requestBody = {
      type: `${getDriverPrefix(source.kind)}_set_function_customization` as const,
      args: {
        source: dataSourceName,
        function: func,
        configuration,
      },
      resource_version,
    };

    return mutate(
      {
        query: requestBody,
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            title: 'Success!',
            message: 'Custom function fields updated successfully',
            type: 'success',
          });
          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (error, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Error',
            error,
          });
          options?.onError?.(error, variables, onMutateResult, context);
        },
      },
    );
  };

  return { ...mutation, setFunctionCustomization };
};
