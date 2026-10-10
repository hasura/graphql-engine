import { useCallback } from 'react';
import {
  MetadataMigrationOptions,
  useMetadataMigration,
  useMetadata,
} from '@hasura/metadata/api';
import { MetadataFunction, TableFunction } from '@hasura/shared/types';
import {
  getDriverPrefix,
  areTablesEqual,
  MetadataSelectors,
} from '@hasura/metadata/helpers';

export type MetadataFunctionPayload = {
  function: TableFunction;
  configuration?: MetadataFunction['configuration'];
  source: string;
  comment?: string;
};

export const useSetFunctionConfiguration = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    onSuccess: (data, variables, onMutateResult, context) => {
      globalMutateOptions?.onSuccess?.(
        data,
        variables,
        onMutateResult,
        context,
      );
    },
  });

  const { data: { driver, resource_version, functions = [] } = {} } =
    useMetadata((m) => ({
      driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
      resource_version: m.resource_version,
      functions: MetadataSelectors.findSource(dataSourceName)(m)?.functions,
    }));

  const setFunctionConfiguration = useCallback(
    ({
      qualifiedFunction,
      configuration,
      ...mutationOptions
    }: {
      qualifiedFunction: TableFunction;
      configuration: MetadataFunction['configuration'];
    } & MetadataMigrationOptions) => {
      const metadataFunction = functions.find((fn) =>
        areTablesEqual(fn.function, qualifiedFunction),
      );

      const payload = {
        type: `${getDriverPrefix(driver ?? 'postgres')}_set_function_customization` as const,
        args: {
          source: dataSourceName,
          function: metadataFunction?.function,
          configuration,
        },
      };

      mutate(
        {
          query: {
            type: 'bulk',
            resource_version,
            args: [payload],
          },
        },
        {
          ...mutationOptions,
        },
      );
    },
    [functions, driver, dataSourceName, mutate, resource_version],
  );

  return { setFunctionConfiguration, ...rest };
};
