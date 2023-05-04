import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { useInvalidateMetadata, useMetadata } from '../../hasura-metadata-api';
import { NativeQuery } from '../../hasura-metadata-types';
import { getSourceDriver } from './utils';

export type TrackNativeQuery = {
  dataSourceName: string;
} & NativeQuery;

export type UntrackNativeQuery = { dataSourceName: string } & Pick<
  NativeQuery,
  'root_field_name'
>;

export const useTrackNativeQuery = (
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

  const trackNativeQuery = async ({
    data: { dataSourceName, ...args },
    ...options
  }: {
    data: TrackNativeQuery;
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${getSourceDriver(
            sources,
            dataSourceName
          )}_track_native_query`,
          args: {
            source: dataSourceName,
            ...args,
          },
        },
      },
      options
    );
  };

  const untrackNativeQuery = async ({
    data: { dataSourceName, ...args },
    ...options
  }: {
    data: UntrackNativeQuery;
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${getSourceDriver(
            sources,
            dataSourceName
          )}_untrack_native_query`,
          args: {
            source: dataSourceName,
            ...args,
          },
        },
      },
      options
    );
  };

  return { trackNativeQuery, untrackNativeQuery, ...rest };
};
