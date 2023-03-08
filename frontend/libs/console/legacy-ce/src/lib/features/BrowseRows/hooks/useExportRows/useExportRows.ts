import { getTableDisplayName } from '../../../DatabaseRelationships';
import { useHttpClient } from '../../../Network';
import { TableRow } from '../../../DataSource';
import { fetchRows, UseRowsPropType } from '../useRows';
import { getFileName } from './useExportRows.utils';
import {
  downloadObjectAsCsvFile,
  downloadObjectAsJsonFile,
} from '../../../../components/Common/utils/export.utils';

export type ExportFileFormat = 'CSV' | 'JSON';

export type UseExportRowsReturn = {
  onExportRows: (
    exportFileFormat: ExportFileFormat
  ) => Promise<TableRow[] | Error>;
};

export const useExportRows = ({
  columns,
  dataSourceName,
  options,
  table,
}: UseRowsPropType): UseExportRowsReturn => {
  const httpClient = useHttpClient();

  const onExportRows = async (
    exportFileFormat: ExportFileFormat
  ): Promise<TableRow[] | Error> => {
    const rows = await fetchRows({
      columns,
      dataSourceName,
      httpClient,
      options,
      table,
    });

    if (!Array.isArray(rows)) {
      throw new Error(rows);
    }

    const fileName = getFileName(getTableDisplayName(table));

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
