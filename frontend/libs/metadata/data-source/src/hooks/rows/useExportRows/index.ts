import { getFileName } from './utils';
import {
  downloadObjectAsCsvFile,
  downloadObjectAsJsonFile,
  getTableDisplayName,
} from '@hasura/shared/utils';
import { useAuthFetchJson } from '@hasura/shared/hooks';
import { UseRowsPropType } from '../useRows';
import { useAppContext } from '@hasura/shared/context';
import { getDatabaseMethods, TableRow } from '../../../driver';

export type ExportFileFormat = 'CSV' | 'JSON';

export type UseExportRowsReturn = {
  onExportRows: (
    filter: UseRowsPropType,
    exportFileFormat: ExportFileFormat,
  ) => Promise<TableRow[] | Error>;
};

export const useExportRows = (): UseExportRowsReturn => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const onExportRows: UseExportRowsReturn['onExportRows'] = async (
    filter: UseRowsPropType,
    exportFileFormat: ExportFileFormat,
  ) => {
    if (!filter.source || !filter.table) {
      return new Error('Source and table are required to export rows');
    }

    const dataSource = getDatabaseMethods(filter.source.kind);

    const rows = await dataSource.query.getTableRows({
      endpoints,
      fetchJson,
      dataSourceName: filter.source.name,
      table: filter.table,
      columns: filter.columns?.length
        ? filter.columns
        : await dataSource.introspection.getTableColumns({
            endpoints,
            fetchJson,
            dataSourceName: filter.source.name,
            table: filter.table,
          }),
      options: filter.options,
    });

    const fileName = getFileName(getTableDisplayName(filter.table));

    if (exportFileFormat === 'JSON') {
      downloadObjectAsJsonFile(fileName, rows);
    } else if (exportFileFormat === 'CSV') {
      downloadObjectAsCsvFile(fileName, rows);
    }

    return rows;
  };

  return {
    onExportRows,
  };
};
