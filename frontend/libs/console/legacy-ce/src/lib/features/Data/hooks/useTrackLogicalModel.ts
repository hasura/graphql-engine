import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { useMetadata } from '../../hasura-metadata-api';
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

  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    errorTransform: transformErrorResponse,
    onSuccess: (data, variable, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variable, ctx);
    },
  });

  const trackLogicalModel = async ({
    data: { dataSourceName, name, fields },
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
            fields,
          },
        },
      },
      options
    );
  };

  const untrackLogicalModel = async ({
    data: { dataSourceName, name, dataSourceKind },
    ...options
  }: {
    data: { dataSourceName: string; name: string; dataSourceKind: string };
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${dataSourceKind}_untrack_logical_model`,
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
