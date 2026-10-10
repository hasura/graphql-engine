import {
  useMetadataMigration,
  MetadataMigrationOptions,
  useMetadataHelpers,
} from '../metadata';
import { getDriverPrefix } from '@hasura/metadata/helpers';
import { Table } from '@hasura/shared/types';

// https://hasura.io/docs/2.0/api-reference/metadata-api/table-view/#metadata-pg-track-table-syntax
export type TrackTableItem = {
  // Name of the table
  table: Table;
  // Name of the source database of the table (default: default)
  source: string;
  // Configuration for the table/view
  configuration?: Record<string, any>;
  logical_model?: string;
  // Apollo federation configuration for the table.
  apollo_federation_config?: {
    enable: 'v1';
  };
};

export function getTrackTableArgs(
  args: TrackTableItem,
  driver: string,
  resource_version?: number,
) {
  return {
    type: `${getDriverPrefix(driver)}_track_table` as const,
    args,
    resource_version,
  };
}

export const useTrackTable = () => {
  const { fetchSource } = useMetadataHelpers();
  const { mutate, ...rest } = useMetadataMigration();

  const trackTable = async (
    args: TrackTableItem,
    options?: MetadataMigrationOptions,
  ) => {
    const { source, resource_version } = await fetchSource(args.source);

    const payload = getTrackTableArgs(args, source.kind, resource_version);

    return mutate(
      {
        query: payload,
      },
      options,
    );
  };

  return {
    trackTable,
    ...rest,
  };
};
