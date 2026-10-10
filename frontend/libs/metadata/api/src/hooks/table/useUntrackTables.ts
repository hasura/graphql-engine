import {
  useMetadataMigration,
  MetadataMigrationOptions,
  useMetadataHelpers,
} from '../metadata';
import { BulkKeepGoingResponse, SupportedDriver } from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export type UntrackTablesArgs = {
  // If set to false, any warnings will cause the API call to fail and no tables to be untracked.
  // Otherwise tables that fail to untrack will be raised as warnings. (default: true)
  allow_warnings?: boolean;
  source: string;
  // An array of untrack table arguments.
  tables: {
    table: unknown;
    cascade?: boolean | undefined;
  }[];
};

export const getUntrackTablesQuery = (
  args: UntrackTablesArgs,
  driver: SupportedDriver,
  resource_version?: number,
) => {
  return {
    type: `${getDriverPrefix(driver)}_untrack_tables` as const,
    args: {
      allow_warnings: args.allow_warnings,
      tables: args.tables.map((table) => ({
        ...table,
        source: args.source,
      })),
    },
    resource_version,
  };
};

export const useUntrackTables = () => {
  const { fetchSource } = useMetadataHelpers();
  const { mutate, ...rest } = useMetadataMigration<BulkKeepGoingResponse>();

  const untrackTables = async (
    args: UntrackTablesArgs,
    options?: MetadataMigrationOptions,
  ) => {
    const { source, resource_version } = await fetchSource(args.source);

    return mutate(
      {
        query: getUntrackTablesQuery(args, source.kind, resource_version),
      },
      options,
    );
  };

  return {
    untrackTables,
    ...rest,
  };
};
