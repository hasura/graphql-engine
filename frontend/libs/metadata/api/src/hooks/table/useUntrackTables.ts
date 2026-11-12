import { useMetadataMigrationMutation, useMetadataHelpers } from '../metadata';
import { SupportedDriver } from '@hasura/shared/types';
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

// Tables that failed to untrack are reported as warnings, unless every table
// failed, in which case the request fails with the warnings in `internal`.
export type UntrackTablesResponse = {
  message: 'success';
  warnings?: {
    code: string;
    message: string;
    name: string;
    type: string;
  }[];
};

export const useUntrackTables = () => {
  const { fetchSource } = useMetadataHelpers();
  // the source is fetched inside the mutation, so that `isPending` covers it
  // and a failure to fetch it reaches `onError`
  return useMetadataMigrationMutation<UntrackTablesResponse, UntrackTablesArgs>(
    async (args) => {
      const { source, resource_version } = await fetchSource(args.source);
      return getUntrackTablesQuery(args, source.kind, resource_version);
    },
  );
};
