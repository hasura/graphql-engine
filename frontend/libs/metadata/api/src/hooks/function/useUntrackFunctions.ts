import { TableFunction } from '@hasura/shared/types';
import { getDriverPrefix, MetadataSelectors } from '@hasura/metadata/helpers';
import {
  MetadataMigrationOptions,
  useMetadataHelpers,
  useMetadataMigration,
} from '../metadata';
import { TMigrationSingleQuery } from '../../api';

export type UntrackFunctionArgs = {
  function: TableFunction;
  source: string;
};

export const getUntrackFunctionQuery = (
  args: UntrackFunctionArgs,
  driver: string,
): TMigrationSingleQuery => {
  const prefix = getDriverPrefix(driver);
  return {
    type: `${prefix}_untrack_function` as const,
    args: args,
  };
};

export const useUntrackFunctions = () => {
  const { mutate, ...rest } = useMetadataMigration();
  const { fetchMetadata } = useMetadataHelpers();

  const untrackFunctions = async (
    args: UntrackFunctionArgs[],
    options: MetadataMigrationOptions,
  ) => {
    const meta = await fetchMetadata();

    const payloads = args.map((fn) => {
      const source = MetadataSelectors.findSource(fn.source)(meta);
      if (!source) {
        throw new Error(`Source ${fn.source} does not exist`);
      }

      return getUntrackFunctionQuery(fn, source.kind);
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

  return { untrackFunctions, ...rest };
};
