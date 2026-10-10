import {
  useMetadataMigration,
  MetadataMigrationOptions,
  useMetadataHelpers,
} from '../metadata';
import { BulkKeepGoingResponse, SupportedDriver } from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { MetadataTable, Table } from '@hasura/shared/types';
import { TrackTableItem } from './useTrackTable';
import { hasuraToast } from '@hasura/shared/ui';
import { useErrorNotification } from '../notification';

export type TrackableTable = {
  /**
   *	Useful for ID'ing unique table rows for checkboxes and solo-updates for
   *	individual rows.
   */
  id: string;

  /**
   *	Self explanatory. Used for the display name.
   */
  name: string;

  /**
   *	A `table` represent the generic json that is used by metadata to reference
   *	a tracked table. Its best to not mess with this object and just treat it like
   *	a black box, passing it back to the server while tracking/untracking
   */
  table: Table;

  /**
   *	Right now, we don't have a strong use case for the UI, but it's handy to have this info
   *	while tracking tables, Like put it on badge under the table. Maybe could be even
   *	used for filtering in the future versions.
   */
  type: string;

  is_tracked: boolean;

  /**
   * Configuration data for a table.
   * Can be present if adding configurationg and tracking an untracked table or if tracked table has configuraiton data
   * In other words, the UI needs this property for is_tracked: false objects so
   * configuration can be added prior to applying changes
   */
  configuration?: MetadataTable['configuration'];
};

type GetTrackTablesArgs = {
  // If set to false, any warnings will cause the API call to fail and no new tables to be tracked.
  // Otherwise tables that fail to track will be raised as warnings. (default: true)
  allow_warnings?: boolean;
  tables: TrackTableItem[];
};

export type UserTrackTablesArgs = {
  // If set to false, any warnings will cause the API call to fail and no new tables to be tracked.
  // Otherwise tables that fail to track will be raised as warnings. (default: true)
  allow_warnings?: boolean;
  source: string;
  tables: Omit<TrackTableItem, 'source'>[];
};

export const getTrackTablesArgs = (
  args: GetTrackTablesArgs,
  driver: SupportedDriver,
  resource_version?: number,
) => {
  return {
    type: `${getDriverPrefix(driver)}_track_tables` as const,
    args,
    resource_version,
  };
};

export function transformTrackableTableArgs(
  trackableTable: TrackableTable,
  source: string,
): TrackTableItem {
  const { logical_model, ...configuration } =
    trackableTable.configuration || {};
  return {
    table: trackableTable.table,
    source,
    configuration,
    logical_model,
  };
}

export const useTrackTables = () => {
  const { fetchSource } = useMetadataHelpers();
  const { mutate, ...rest } = useMetadataMigration<BulkKeepGoingResponse>();
  const showErrorNotification = useErrorNotification();

  const trackTables = async (
    args: UserTrackTablesArgs,
    options?: MetadataMigrationOptions<BulkKeepGoingResponse>,
  ) => {
    const { source, resource_version } = await fetchSource(args.source);

    const payload = getTrackTablesArgs(
      {
        allow_warnings: args.allow_warnings,
        tables: args.tables.map((table) => ({
          ...table,
          source: args.source,
        })),
      },
      source.kind,
      resource_version,
    );

    return mutate(
      {
        query: payload,
      },
      {
        ...options,
        onSuccess: (data, variables, onMutateResult, context) => {
          hasuraToast({
            type: 'success',
            title: 'Success',
            message: 'Existing table/view added',
          });
          options?.onSuccess?.(data, variables, onMutateResult, context);
        },
        onError: (error, variables, onMutateResult, context) => {
          showErrorNotification({
            title: 'Adding existing table/view failed',
            error,
          });

          options?.onError?.(error, variables, onMutateResult, context);
        },
      },
    );
  };

  return {
    ...rest,
    trackTables,
  };
};
