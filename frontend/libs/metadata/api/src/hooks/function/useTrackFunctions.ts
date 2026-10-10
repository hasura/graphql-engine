import { getTrackFunctionQuery, TrackFunctionArgs } from './useTrackFunction';
import { MetadataSelectors } from '@hasura/metadata/helpers';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';

export const useTrackFunctions = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchMetadata } = useMetadataHelpers();

  const trackFunctions = async (
    args: TrackFunctionArgs[],
    options?: MetadataMigrationOptions,
  ) => {
    const meta = await fetchMetadata();

    const payloads = args.map((fn) => {
      const source = MetadataSelectors.findSource(fn.source)(meta);
      if (!source) {
        throw new Error(`Source ${fn.source} does not exist`);
      }

      return getTrackFunctionQuery(fn, source.kind);
    });

    return mutate(
      {
        query: {
          type: 'bulk',
          args: payloads,
          resource_version: meta.resource_version,
        },
      },
      options,
    );
  };

  return { trackFunctions, ...rest };
};
