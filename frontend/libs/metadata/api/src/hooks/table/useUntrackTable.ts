import { useMetadataMigrationMutation } from '../metadata';
import {
  QualifiedDataSource,
  SupportedDriver,
  Table,
} from '@hasura/shared/types';
import { getDriverPrefix } from '@hasura/metadata/helpers';

export type UntrackTableArgs = {
  // Name of the table
  table: Table;
  // Name of the source database of the table (default: default)
  source: string;
  // When set to true, the effect (if possible) is cascaded to any Metadata dependent objects (relationships, permissions, templates). (default: false)
  cascade?: boolean;
};

export const getUntrackTableQuery = (
  args: UntrackTableArgs,
  driver: SupportedDriver,
  resource_version?: number,
) => {
  return {
    type: `${getDriverPrefix(driver)}_untrack_table` as const,
    args,
    resource_version,
  };
};

export const useUntrackTable = () => {
  return useMetadataMigrationMutation(
    async ({
      source,
      table,
      cascade,
    }: {
      source: QualifiedDataSource;
      table: Table;
      cascade?: boolean;
    }) => {
      return getUntrackTableQuery(
        {
          source: source.name,
          table,
          cascade,
        },
        source.kind,
      );
    },
  );
};
