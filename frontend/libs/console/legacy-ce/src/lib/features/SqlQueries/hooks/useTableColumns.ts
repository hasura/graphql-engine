import { Api } from '../../../hooks/apiUtils';
import { QualifiedTable } from '../../../metadata/types';
import {
  MetadataSelector,
  useMetadata,
  useMetadataVersion,
} from '../../MetadataAPI';
import { useQuery, UseQueryResult } from 'react-query';
import { useAppSelector } from '../../../storeHooks';
import { getRunSqlQuery } from '../../../components/Common/utils/v1QueryUtils';
import Endpoints from '../../../Endpoints';
import { RunSQLResponse } from '../../../hooks/types';
import { dataSourceSqlQueries } from '..';

export const useTableColumns = (
  database: string,
  table: QualifiedTable
): UseQueryResult<Record<string, any>, Error> => {
  const { data: source } = useMetadata(
    MetadataSelector.getDataSourceMetadata(database)
  );

  const { data: version } = useMetadataVersion();

  const driver = source?.kind ?? '';

  const headers = useAppSelector(state => state.tables.dataHeaders);

  type TableColumn = {
    database: string;
    table_schema: string;
    table_name: string;
    column_name: string;
    data_type: string;
  };

  const parser = (data: Array<string[]>) => {
    if (!data) throw Error('No data found for table!');

    if (data.length < 1) throw Error('Table has no columns');

    const keys = data[0];

    const result: TableColumn[] = [];

    data.slice(1).forEach(row => {
      const obj: Record<string, string> = {};

      keys.forEach((key, i) => {
        obj[key] = row[i];
      });

      result.push(obj as TableColumn);
    });

    return data;
  };

  return useQuery({
    queryKey: ['columns', database, table, driver, version],
    queryFn() {
      if (!driver) throw Error('driver information not found!');

      const sql = dataSourceSqlQueries[driver].getTableColumnsSql(table);

      if (sql === 'not implemented')
        throw Error('feature not supported by driver');

      const body = getRunSqlQuery(sql, database, false, true, driver);

      return Api.post<RunSQLResponse, Record<string, any>>(
        {
          url: Endpoints.query,
          headers,
          body,
        },
        d => parser(d.result ?? [])
      );
    },
    enabled: !!driver && !!table.name,
  });
};
