import { getTableDisplayName } from '@/features/DatabaseRelationships';
import { useHttpClient } from '@/features/Network';
import { fetchRows, UseRowsPropType } from '../useRows';
import { getFileName } from './useExportRows.utils';
import {
  downloadObjectAsCsvFile,
  downloadObjectAsJsonFile,
} from '../../../../components/Common/utils/export.utils';

export type UseExportRowsProps = {
  exportFileFormat: 'CSV' | 'JSON';
} & UseRowsPropType;

export const useExportRows = ({
  columns,
  dataSourceName,
  exportFileFormat,
  options,
  table,
}: UseExportRowsProps) => {
  const httpClient = useHttpClient();

  const onExportRows = async () =>
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

      reject(
        new Error(`Unexpected fetch rows result: ${JSON.stringify(rows)}`)
      );
    });

  return {
    onExportRows,
  };
};
