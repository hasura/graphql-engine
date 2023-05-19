import { useMetadataMigration } from '../../MetadataAPI';
import { useCallback } from 'react';
import {
  MetadataSelectors,
  useInvalidateMetadata,
  useMetadata,
} from '../../hasura-metadata-api';
import type { TrackableTable } from '../TrackResources/types';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
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
            type: `${driver}_track_tables`,
            resource_version,
            args: {
              allow_warnings: true,
              tables: tablesToBeTracked.map(trackableTable => ({
                table: trackableTable.table,
                source: dataSourceName,
                configuration: trackableTable.configuration,
              })),
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
      tablesToBeUntracked,
      ...mutateOptions
    }: {
      tablesToBeUntracked: TrackableTable[];
    } & MetadataMigrationOptions) => {
      mutate(
        {
          query: {
            type: `${driver}_untrack_tables`,
            resource_version,
            args: {
              allow_warnings: true,
              tables: tablesToBeUntracked.map(untrackableTable => ({
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
