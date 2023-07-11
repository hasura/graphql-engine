import { useCallback } from 'react';
import {
  MetadataMigrationOptions,
  useMetadataMigration,
} from './useMetadataMigration';
import {
  MetadataSelectors,
  useInvalidateMetadata,
  useMetadata,
} from '../../hasura-metadata-api';
import { transformErrorResponse } from '../../Data/errorUtils';
import { QualifiedFunction } from '../../hasura-metadata-types';

export const useManageFunctionPermission = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const invalidateMetadata = useInvalidateMetadata();

  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    onSuccess: (data, variables, ctx) => {
      invalidateMetadata({
        componentName: 'UDF Permissions Editor',
        reasons: ['Updated UDF permissions.'],
      });
      globalMutateOptions?.onSuccess?.(data, variables, ctx);
    },
    errorTransform: transformErrorResponse,
  });

  const { data: { driver, resource_version } = {} } = useMetadata(m => ({
    driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
    resource_version: m.resource_version,
  }));

  const createFunctionPermission = useCallback(
    ({
      qualifiedFunction,
      role,
      ...mutationOptions
    }: {
      qualifiedFunction: QualifiedFunction;
      role: string;
    } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: 'bulk',
            source: dataSourceName,
            args: [
              {
                type: `${driver}_create_function_permission`,
                args: {
                  function: qualifiedFunction,
                  role,
                  source: dataSourceName,
                },
              },
            ],
            resource_version,
          },
        },
        {
          ...mutationOptions,
        }
      );
    },
    [mutate, dataSourceName, resource_version, driver]
  );

  const deleteFunctionPermission = useCallback(
    ({
      qualifiedFunction,
      role,
      ...mutationOptions
    }: {
      qualifiedFunction: QualifiedFunction;
      role: string;
    } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: 'bulk',
            source: dataSourceName,
            args: [
              {
                type: `${driver}_drop_function_permission`,
                args: {
                  function: qualifiedFunction,
                  role,
                  source: dataSourceName,
                },
              },
            ],
            resource_version,
          },
        },
        {
          ...mutationOptions,
        }
      );
    },
    [mutate, dataSourceName, resource_version, driver]
  );

  return { createFunctionPermission, deleteFunctionPermission, ...rest };
};
