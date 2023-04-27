import { useMetadataMigration } from '../../MetadataAPI';
import { useCallback } from 'react';
import {
  MetadataSelectors,
  useInvalidateMetadata,
  useMetadata,
} from '../../hasura-metadata-api';
import type { TrackableTable } from '../TrackResources/types';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { BulkKeepGoingResponse } from '../../hasura-metadata-types';

export const useTrackTables = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const { data: { driver, resource_version } = {} } = useMetadata(m => ({
    driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
    resource_version: m.resource_version,
  }));

  const invalidateMetadata = useInvalidateMetadata();

  const { mutate, ...rest } = useMetadataMigration<BulkKeepGoingResponse>({
    ...globalMutateOptions,
    onSuccess: (data, variables, ctx) => {
      invalidateMetadata();
      globalMutateOptions?.onSuccess?.(data, variables, ctx);
    },
  });

  const trackTables = useCallback(
    ({
      tablesToBeTracked,
      ...mutateOptions
    }: {
      tablesToBeTracked: TrackableTable[];
    } & MetadataMigrationOptions<BulkKeepGoingResponse>) => {
      mutate(
        {
          query: {
            type: 'bulk_keep_going',
            source: dataSourceName,
            resource_version,
            args: tablesToBeTracked.map(trackableTable => ({
              type: `${driver}_track_table`,
              args: {
                table: trackableTable.table,
                source: dataSourceName,
                configuration: trackableTable.configuration,
              },
            })),
          },
        },
        mutateOptions
      );
    },
    [dataSourceName, driver, mutate, resource_version]
  );

  const untrackTables = useCallback(
    ({
      tablesToBeTracked,
      ...mutateOptions
    }: {
      tablesToBeTracked: TrackableTable[];
    } & MetadataMigrationOptions<BulkKeepGoingResponse>) => {
      mutate(
        {
          query: {
            type: 'bulk_keep_going',
            source: dataSourceName,
            resource_version,
            args: tablesToBeTracked.map(trackableTable => ({
              type: `${driver}_untrack_table`,
              args: {
                table: trackableTable.table,
                source: dataSourceName,
                // This will remove any relationships that are attached to the table
                cascade: true,
              },
            })),
          },
        },
        mutateOptions
      );
    },
    [dataSourceName, driver, mutate, resource_version]
  );

  return { trackTables, untrackTables, ...rest };
};
