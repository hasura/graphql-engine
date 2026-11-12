import { getFileName } from './utils';
import {
  CSVExportOptions,
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
    csvOptions?: Partial<CSVExportOptions>,
  ) => Promise<TableRow[] | Error>;
};

export const useExportRows = (): UseExportRowsReturn => {
  const { endpoints } = useAppContext();
  const fetchJson = useAuthFetchJson();

  const onExportRows: UseExportRowsReturn['onExportRows'] = async (
    filter: UseRowsPropType,
    exportFileFormat: ExportFileFormat,
    csvOptions?: Partial<CSVExportOptions>,
  ) => {
    if (!filter.source || !filter.table) {
      return new Error('Source and table are required to export rows');
    }

    const dataSource = getDatabaseMethods(filter.source.kind);

    const columns = filter.columns?.length
      ? filter.columns
      : await dataSource.introspection.getTableColumns({
          endpoints,
          fetchJson,
          dataSourceName: filter.source.name,
          table: filter.table,
        });

    const rows = await dataSource.query.getTableRows({
      endpoints,
      fetchJson,
      dataSourceName: filter.source.name,
      table: filter.table,
      columns,
      options: filter.options,
    });

    const fileName = getFileName(getTableDisplayName(filter.table));

    if (exportFileFormat === 'JSON') {
      downloadObjectAsJsonFile(fileName, rows);
    } else if (exportFileFormat === 'CSV') {
      // the column names give an empty export its header row
      downloadObjectAsCsvFile(
        fileName,
        rows,
        csvOptions,
        columns.map((column) => column.name),
      );
    }

    return rows;
  };

  return {
    onExportRows,
  };
};
