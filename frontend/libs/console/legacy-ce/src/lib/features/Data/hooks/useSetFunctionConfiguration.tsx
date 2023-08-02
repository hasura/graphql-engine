import { useCallback } from 'react';
import {
  MetadataMigrationOptions,
  useMetadataMigration,
} from '../../MetadataAPI/hooks/useMetadataMigration';
import {
  MetadataSelectors,
  areTablesEqual,
  useMetadata,
} from '../../hasura-metadata-api';
import {
  MetadataFunction,
  QualifiedFunction,
} from '../../hasura-metadata-types';
import { transformErrorResponse } from '../errorUtils';

export type MetadataFunctionPayload = {
  function: QualifiedFunction;
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
    onSuccess: (data, variables, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variables, ctx);
    },
    errorTransform: transformErrorResponse,
  });

  const { data: { driver, resource_version, functions = [] } = {} } =
    useMetadata(m => ({
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
      qualifiedFunction: QualifiedFunction;
      configuration: MetadataFunction['configuration'];
    } & MetadataMigrationOptions) => {
      const metadataFunction = functions.find(fn =>
        areTablesEqual(fn.function, qualifiedFunction)
      );

      const payload = {
        type: `${driver}_set_function_customization`,
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
            source: dataSourceName,
            resource_version,
            args: [payload],
          },
        },
        {
          ...mutationOptions,
        }
      );
    },
    [functions, driver, dataSourceName, mutate, resource_version]
  );

  return { setFunctionConfiguration, ...rest };
};
