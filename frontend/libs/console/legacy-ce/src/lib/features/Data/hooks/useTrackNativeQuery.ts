import {
  useMetadataMigration,
  MetadataMigrationOptions,
  useMetadataHelpers,
} from '@hasura/metadata/api';
import { NativeQuery } from '@hasura/shared/types';
import { NativeQueryMigrationBuilder } from '../LogicalModels/MigrationBuilder';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export type TrackNativeQuery = {
  source: string;
} & NativeQuery;

export type UntrackNativeQuery = {
  sourceName: string;
  rootFieldName: string;
};

export const useTrackNativeQuery = (
  globalMutateOptions?: MetadataMigrationOptions,
) => {
  const { fetchSource } = useMetadataHelpers();
  const { mutate, ...rest } = useMetadataMigration({
    ...globalMutateOptions,
    onSuccess: (data, variable, onMutateResult, context) => {
      globalMutateOptions?.onSuccess?.(data, variable, onMutateResult, context);
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
    const { source: selectedSource, resource_version } =
      await fetchSource(source);

    const builder = new NativeQueryMigrationBuilder({
      dataSourceName: source,
      driver: selectedSource.kind,
      nativeQuery,
    });

    // we need the untrack command to use the old root_field_name
    // if a user is editing the native query, there's a chance that the root field name changed
    // so, we have to manually set that when using untrack()
    const argz = editDetails
      ? builder.untrack(editDetails.rootFieldName).track().payload()
      : builder.track().payload();

    return mutate(
      {
        query: {
          resource_version,
          type: 'bulk_atomic',
          args: argz,
        },
      },
      options,
    );
  };

  const untrackNativeQuery = async (
    args: UntrackNativeQuery,
    options?: MetadataMigrationOptions,
  ) => {
    const { source, resource_version } = await fetchSource(args.sourceName);

    return mutate(
      {
        query: {
          resource_version,
          type: `${getDriverPrefix(source.kind)}_untrack_native_query` as const,
          args: {
            source: source.name,
            root_field_name: args.rootFieldName,
          },
        },
      },
      options,
    );
  };

  return { trackNativeQuery, untrackNativeQuery, ...rest };
};
