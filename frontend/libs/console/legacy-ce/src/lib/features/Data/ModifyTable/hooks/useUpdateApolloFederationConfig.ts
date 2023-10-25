import { useCallback } from 'react';
import { MetadataSelectors, useMetadata } from '../../../hasura-metadata-api';
import { useMetadataMigration } from '../../../MetadataAPI';
import { MetadataMigrationOptions } from '../../../MetadataAPI/hooks/useMetadataMigration';
import { transformErrorResponse } from '../../errorUtils';
import { Table } from '../../../hasura-metadata-types';

export const useUpdateApolloFederationConfig = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const { data: { driver, resource_version } = {} } = useMetadata(m => ({
    driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
    resource_version: m.resource_version,
  }));

  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    onSuccess: (data, variables, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variables, ctx);
    },
    errorTransform: transformErrorResponse,
  });

  const updateApolloConfig = useCallback(
    ({
      table,
      isEnabled,
      ...mutateOptions
    }: {
      table: Table;
      isEnabled: boolean;
    } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: `${driver}_set_apollo_federation_config`,
            resource_version,
            args: {
              table,
              source: dataSourceName,
              apollo_federation_config: isEnabled
                ? {
                    enable: 'v1',
                  }
                : null,
            },
          },
        },
        mutateOptions
      );
    },
    [dataSourceName, driver, mutate, resource_version]
  );

  return {
    updateApolloConfig,
    ...rest,
  };
};
