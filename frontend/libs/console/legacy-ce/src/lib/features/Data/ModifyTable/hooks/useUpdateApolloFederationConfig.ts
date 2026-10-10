import { useCallback } from 'react';
import {
  useMetadata,
  useMetadataMigration,
  MetadataMigrationOptions,
} from '@hasura/metadata/api';
import { Table } from '@hasura/shared/types';
import { getDriverPrefix, MetadataSelectors } from '@hasura/metadata/helpers';

export const useUpdateApolloFederationConfig = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const { data: { driver, resource_version } = {} } = useMetadata((m) => ({
    driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
    resource_version: m.resource_version,
  }));

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
            type: `${getDriverPrefix(driver ?? 'postgres')}_set_apollo_federation_config`,
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
        mutateOptions,
      );
    },
    [dataSourceName, driver, mutate, resource_version],
  );

  return {
    updateApolloConfig,
    ...rest,
  };
};
