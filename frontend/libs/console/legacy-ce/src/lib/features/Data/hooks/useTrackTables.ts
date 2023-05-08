import { TMigration, useMetadataMigration } from '../../MetadataAPI';
import { useCallback } from 'react';
import {
  MetadataSelectors,
  useInvalidateMetadata,
  useMetadata,
} from '../../hasura-metadata-api';
import type { TrackableTable } from '../TrackResources/types';
import {
  MAX_METADATA_BATCH_SIZE,
  MetadataMigrationOptions,
} from '../../MetadataAPI/hooks/useMetadataMigration';
import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { BulkKeepGoingResponse } from '../../hasura-metadata-types';
import chunk from 'lodash/chunk';

type BatchMigrationOptions = Omit<MetadataMigrationOptions, 'onSuccess'> & {
  onSuccess?: (
    data: Record<string, any>,
    variables: TMigration<Record<string, any>>,
    context: unknown,
    batchInfo: {
      totalBatchSize: number;
      batchNumber: number;
      aggregatedResults: Record<string, any>[];
    }
  ) => void;
};

export const useTrackTables = ({
  dataSourceName,
  ...globalMutateOptions
}: { dataSourceName: string } & MetadataMigrationOptions) => {
  const { data: { driver, resource_version } = {} } = useMetadata(m => ({
    driver: MetadataSelectors.findSource(dataSourceName)(m)?.kind,
    resource_version: m.resource_version,
  }));

  const invalidateMetadata = useInvalidateMetadata();

  const { mutate, mutateAsync, ...rest } =
    useMetadataMigration<BulkKeepGoingResponse>({
      ...globalMutateOptions,
      onSuccess: (data, variables, ctx) => {
        invalidateMetadata();
        globalMutateOptions?.onSuccess?.(data, variables, ctx);
      },
      errorTransform: transformErrorResponse,
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

  const trackTablesInBatches = useCallback(
    async ({
      tablesToBeTracked,
      ...mutateOptions
    }: { tablesToBeTracked: TrackableTable[] } & BatchMigrationOptions) => {
      const results: Record<string, any>[] = [];
      const batches = chunk(tablesToBeTracked, MAX_METADATA_BATCH_SIZE);
      for (const [index, batch] of batches.entries()) {
        await mutateAsync(
          {
            query: {
              type: 'bulk_keep_going',
              source: dataSourceName,
              args: batch.map(trackableTable => ({
                type: `${driver}_track_table`,
                args: {
                  table: trackableTable.table,
                  source: dataSourceName,
                  configuration: trackableTable.configuration,
                },
              })),
            },
          },
          {
            ...mutateOptions,
            onSuccess: (data, variables, ctx) => {
              results.push(data);
              return mutateOptions?.onSuccess?.(data, variables, ctx, {
                totalBatchSize: batches.length,
                batchNumber: index + 1,
                aggregatedResults: results.flat(),
              });
            },
          }
        );
      }
    },
    [dataSourceName, driver, mutateAsync]
  );

  const untrackTables = useCallback(
    ({
      tablesToBeUntracked,
      ...mutateOptions
    }: {
      tablesToBeUntracked: TrackableTable[];
    } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: 'bulk_keep_going',
            source: dataSourceName,
            resource_version,
            args: tablesToBeUntracked.map(trackedTable => ({
              type: `${driver}_untrack_table`,
              args: {
                table: trackedTable.table,
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

  const untrackTablesInBatches = useCallback(
    async ({
      tablesToBeUntracked,
      ...mutateOptions
    }: { tablesToBeUntracked: TrackableTable[] } & BatchMigrationOptions) => {
      // const { onFinalError, onFinalSuccess, ...options } = mutateOptions;
      const results: Record<string, any>[] = [];
      const batches = chunk(tablesToBeUntracked, MAX_METADATA_BATCH_SIZE);
      for (const [index, batch] of batches.entries()) {
        await mutateAsync(
          {
            query: {
              type: 'bulk_keep_going',
              source: dataSourceName,
              args: batch.map(trackedTable => ({
                type: `${driver}_untrack_table`,
                args: {
                  table: trackedTable.table,
                  source: dataSourceName,
                  // This will remove any relationships that are attached to the table
                  cascade: true,
                },
              })),
            },
          },
          {
            ...mutateOptions,
            onSuccess: (data, variables, ctx) => {
              return mutateOptions?.onSuccess?.(data, variables, ctx, {
                totalBatchSize: batches.length,
                batchNumber: index + 1,
                aggregatedResults: [
                  ...results,
                  ...(data as Record<string, any>[]),
                ].flat(),
              });
            },
          }
        );
      }
    },
    [dataSourceName, driver, mutateAsync]
  );

  return {
    trackTables,
    untrackTables,
    trackTablesInBatches,
    untrackTablesInBatches,
    ...rest,
  };
};
