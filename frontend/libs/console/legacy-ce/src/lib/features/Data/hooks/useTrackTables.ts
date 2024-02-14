import { useCallback } from 'react';
import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { allowedMetadataTypes, useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { MetadataSelectors, useMetadata } from '../../hasura-metadata-api';
import { BulkKeepGoingResponse } from '../../hasura-metadata-types';
import type { TrackableTable } from '../TrackResources/types';

type GetTrackTablesPayloadArgs = {
  dataSourceName: string;
  tables: TrackableTable[];
  driver: string | undefined;
};

export const getTrackTablesPayload = ({
  dataSourceName,
  tables,
  driver,
}: GetTrackTablesPayloadArgs) => {
  return {
    type: `${driver}_track_tables` as allowedMetadataTypes,
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
  };
};

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
      const payload = getTrackTablesPayload({
        dataSourceName,
        tables,
        driver,
      });

      mutate(
        {
          query: payload,
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
    getTrackTablesPayload,
    ...rest,
  };
};
