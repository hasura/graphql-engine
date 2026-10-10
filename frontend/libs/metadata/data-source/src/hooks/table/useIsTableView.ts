import { Source, Table } from '@hasura/shared/types';
import { areTablesEqual } from '@hasura/metadata/helpers';
import { getDatabaseMethods } from '../../driver';
import { useTrackableTables } from './useTrackableTables';
import { defaultQueryOptions } from '@hasura/metadata/api';

export const useIsTableView = ({
  source,
  table,
}: {
  source: Source | undefined;
  table: Table | undefined;
}) => {
  return useTrackableTables<boolean>(
    {
      source,
    },
    {
      select: (data) => {
        if (!source || !table) {
          return false;
        }

        const databaseMethods = getDatabaseMethods(source?.kind ?? '');
        const result = data.find((t) => areTablesEqual(t.table, table!));

        return (
          result?.type !== undefined &&
          !databaseMethods.check.isTable(result.type)
        );
      },
      ...defaultQueryOptions,
      enabled: Boolean(source),
    },
  );
};
