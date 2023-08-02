import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { useMetadata } from '../../hasura-metadata-api';
import {
  QualifiedStoredProcedure,
  StoredProcedure,
} from '../../hasura-metadata-types';
import { getSourceDriver } from './utils';

export type TrackStoredProcedure = {
  dataSourceName: string;
} & StoredProcedure;

export const useTrackStoredProcedure = (
  globalMutateOptions?: MetadataMigrationOptions
) => {
  /**
   * Get the required metadata variables - sources & resource_version
   */
  const { data: { sources = [], resource_version } = {} } = useMetadata(m => ({
    sources: m.metadata.sources,
    resource_version: m.resource_version,
  }));

  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    errorTransform: transformErrorResponse,
    onSuccess: (data, variable, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variable, ctx);
    },
  });

  const trackStoredProcedure = async ({
    data: { dataSourceName, ...otherArgs },
    ...options
  }: {
    data: TrackStoredProcedure;
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${getSourceDriver(
            sources,
            dataSourceName
          )}_track_stored_procedure`,
          args: {
            source: dataSourceName,
            ...otherArgs,
          },
        },
      },
      options
    );
  };

  const untrackStoredProcedure = async ({
    data: { dataSourceName, stored_procedure },
    ...options
  }: {
    data: {
      dataSourceName: string;
      stored_procedure: QualifiedStoredProcedure;
    };
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${getSourceDriver(
            sources,
            dataSourceName
          )}_untrack_stored_procedure`,
          args: {
            source: dataSourceName,
            stored_procedure,
          },
        },
      },
      options
    );
  };

  return { trackStoredProcedure, untrackStoredProcedure, ...rest };
};
