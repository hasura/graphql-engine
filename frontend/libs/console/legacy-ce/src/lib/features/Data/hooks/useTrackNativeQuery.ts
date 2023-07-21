import { transformErrorResponse } from '../../ConnectDBRedesign/utils';
import { useMetadataMigration } from '../../MetadataAPI';
import { MetadataMigrationOptions } from '../../MetadataAPI/hooks/useMetadataMigration';
import { useMetadata } from '../../hasura-metadata-api';
import { NativeQuery, Source } from '../../hasura-metadata-types';
import { NativeQueryMigrationBuilder } from '../LogicalModels/MigrationBuilder';
import { getSourceDriver } from './utils';

export type TrackNativeQuery = {
  source: string;
} & NativeQuery;

export type UntrackNativeQuery = { source: Source } & Pick<
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

  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    errorTransform: transformErrorResponse,
    onSuccess: (data, variable, ctx) => {
      globalMutateOptions?.onSuccess?.(data, variable, ctx);
    },
  });

  const trackNativeQuery = async ({
    data: args,
    editDetails,
    ...options
  }: {
    data: TrackNativeQuery;
    editDetails?: { rootFieldName: string };
  } & MetadataMigrationOptions) => {
    const { source, ...nativeQuery } = args;
    const driver = getSourceDriver(sources, args.source);

    if (!driver) {
      throw new Error('Source could not be found. Unable to identify driver.');
    }

    const builder = new NativeQueryMigrationBuilder({
      dataSourceName: source,
      driver,
      nativeQuery,
    });

    // we need the untrack command to use the old root_field_name
    // if a user is editing the native query, there's a chance that the root field name changed
    // so, we have to manually set that when using untrack()
    const argz = editDetails
      ? builder.untrack(editDetails.rootFieldName).track().payload()
      : builder.track().payload();

    mutate(
      {
        query: {
          resource_version,
          type: 'bulk_atomic',

          args: argz,
        },
      },
      options
    );
  };

  const untrackNativeQuery = async ({
    data: args,
    ...options
  }: {
    data: UntrackNativeQuery;
  } & MetadataMigrationOptions) => {
    mutate(
      {
        query: {
          resource_version,
          type: `${args.source.kind}_untrack_native_query`,
          args: {
            source: args.source.name,
            root_field_name: args.root_field_name,
          },
        },
      },
      options
    );
  };

  return { trackNativeQuery, untrackNativeQuery, ...rest };
};
