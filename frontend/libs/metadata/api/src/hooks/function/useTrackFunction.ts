import { TableFunction, MetadataFunction } from '@hasura/shared/types';
import { getDriverPrefix, MetadataSelectors } from '@hasura/metadata/helpers';
import { TMigrationSingleQuery } from '../../api';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';

export type TrackFunctionArgs = {
  function: TableFunction;
  configuration?: MetadataFunction['configuration'];
  source: string;
  comment?: string;
};

export const getTrackFunctionQuery = (
  args: TrackFunctionArgs,
  driver: string,
  resourceVersion?: number,
): TMigrationSingleQuery => {
  const prefix = getDriverPrefix(driver);
  return {
    type: `${prefix}_track_function` as const,
    args: args,
    resource_version: resourceVersion,
  };
};

export const useTrackFunction = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchMetadata } = useMetadataHelpers();

  const trackFunction = async (
    args: TrackFunctionArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const meta = await fetchMetadata();
    const source = MetadataSelectors.findSource(args.source)(meta);
    if (!source) {
      throw new Error(`Source ${args.source} does not exist`);
    }

    return mutate(
      {
        query: getTrackFunctionQuery(args, source.kind, meta.resource_version),
      },
      options,
    );
  };

  return { trackFunction, ...rest };
};
