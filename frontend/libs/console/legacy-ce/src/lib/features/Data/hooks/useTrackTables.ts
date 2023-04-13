import { useMetadataMigration } from '../../MetadataAPI';
import { useCallback } from 'react';
import {
  MetadataSelectors,
  useInvalidateMetadata,
  useMetadata,
} from '../../hasura-metadata-api';
import type { TrackableTable } from '../TrackResources/types';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';

export const useTrackTables = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const { data: { driver, resource_version } = {} } = useMetadata(m => ({
    driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
    resource_version: m.resource_version,
  }));

  const invalidateMetadata = useInvalidateMetadata();

  const { mutate, ...rest } = useMetadataMigration({
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
    }: { tablesToBeTracked: TrackableTable[] } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: 'bulk',
            source: dataSourceName,
            resource_version,
            args: tablesToBeTracked.map(trackableTable => ({
              type: `${driver}_track_table`,
              args: {
                table: trackableTable.table,
                source: dataSourceName,
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
    }: { tablesToBeTracked: TrackableTable[] } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: 'bulk',
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
