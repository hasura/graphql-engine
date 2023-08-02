import { useCallback } from 'react';
import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { BulkKeepGoingResponse } from '../../hasura-metadata-types';
import type { TrackableTable } from '../TrackResources/types';

export const useTrackTables = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const { data: { driver, resource_version } = {} } = useMetadata(m => ({
    driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
    resource_version: m.resource_version,
  }));

  const { mutate, ...rest } = useMetadataMigration<BulkKeepGoingResponse>({
    ...globalMutateOptions,
    onSuccess: (data, variables, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variables, ctx);
    },
    errorTransform: transformErrorResponse,
  });

  const trackTables = useCallback(
    ({
      tables,
      ...mutateOptions
    }: {
      tables: TrackableTable[];
    } & MetadataMigrationOptions<BulkKeepGoingResponse>) => {
      mutate(
        {
          query: {
            type: `${driver}_track_tables`,
            resource_version,
            args: {
              allow_warnings: true,
              tables: tables.map(trackableTable => {
                const { logical_model, ...configuration } =
                  trackableTable.configuration || {};
                return {
                  table: trackableTable.table,
                  source: dataSourceName,
                  configuration,
                  logical_model,
                };
              }),
            },
          },
        },
        mutateOptions
      );
    },
    [dataSourceName, driver, mutate, resource_version]
  );

  const untrackTables = useCallback(
    ({
      tables,
      ...mutateOptions
    }: {
      tables: TrackableTable[];
    } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: `${driver}_untrack_tables`,
            resource_version,
            args: {
              allow_warnings: true,
              tables: tables.map(untrackableTable => ({
                table: untrackableTable.table,
                source: dataSourceName,
                configuration: untrackableTable.configuration,
              })),
            },
          },
        },
        mutateOptions
      );
    },
    [dataSourceName, driver, mutate, resource_version]
  );

  return {
    trackTables,
    untrackTables,
    ...rest,
  };
};
