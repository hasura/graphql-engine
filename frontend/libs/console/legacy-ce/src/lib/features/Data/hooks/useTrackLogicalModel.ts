import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { useInvalidateMetadata, useMetadata } from '../../hasura-metadata-api';
import { LogicalModel } from '../../hasura-metadata-types';
import { getSourceDriver } from './utils';

export type TrackLogicalModel = {
  dataSourceName: string;
} & LogicalModel;

export const useTrackLogicalModel = (
  globalMutateOptions?: MetadataMigrationOptions
) => {
  /**
   * Get the required metadata variables - sources & resource_version
   */
  const { data: { sources = [], resource_version } = {} } = useMetadata(m => ({
    sources: m.metadata.sources,
    resource_version: m.resource_version,
  }));

  const invalidateMetadata = useInvalidateMetadata();

  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    errorTransform: transformErrorResponse,
    onSuccess: (data, variable, ctx) => {
      invalidateMetadata();
      globalMutateOptions?.onSuccess?.(data, variable, ctx);
    },
  });

  const trackLogicalModel = async ({
    data: { dataSourceName, name, cardinality, fields },
    ...options
  }: {
    data: TrackLogicalModel;
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${getSourceDriver(
            sources,
            dataSourceName
          )}_track_logical_model`,
          args: {
            source: dataSourceName,
            name,
            cardinality,
            fields,
          },
        },
      },
      options
    );
  };

  const untrackLogicalModel = async ({
    data: { dataSourceName, name },
    ...options
  }: {
    data: { dataSourceName: string; name: string };
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${getSourceDriver(
            sources,
            dataSourceName
          )}_untrack_logical_model`,
          args: {
            source: dataSourceName,
            name,
          },
        },
      },
      options
    );
  };

  return { trackLogicalModel, untrackLogicalModel, ...rest };
};
