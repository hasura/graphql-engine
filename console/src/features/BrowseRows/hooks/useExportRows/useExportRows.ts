import { getTableDisplayName } from '@/features/DatabaseRelationships';
import { useHttpClient } from '@/features/Network';
import { TableRow } from '@/features/DataSource';
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
  ): Promise<TableRow[] | Error> =>
    new Promise(async (resolve, reject) => {
      const rows = await fetchRows({
        columns,
        dataSourceName,
        httpClient,
        options,
        table,
      });

      if (Array.isArray(rows)) {
        const fileName = getFileName(getTableDisplayName(table));

        if (exportFileFormat === 'JSON') {
          downloadObjectAsJsonFile(fileName, rows);
        } else if (exportFileFormat === 'CSV') {
          downloadObjectAsCsvFile(fileName, rows);
        }

        resolve(rows);
        return;
      }

      reject(new Error(rows));
    });

  return {
    onExportRows,
  };
};
