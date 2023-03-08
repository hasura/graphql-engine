import { getRunSqlQuery } from '../../../../Common/utils/v1QueryUtils';
import { dataSource } from '../../../../../dataSources';
import Endpoints from '../../../../../Endpoints';
import { useHttpClient } from '../../../../../features/Network';
import { useQuery } from 'react-query';

export type UseTableRelationsType = {
  dataSourceName: string;
  schemaName: string;
};

export const useSchemas = ({
  dataSourceName,
  schemaName,
}: UseTableRelationsType) => {
  const httpClient = useHttpClient();

  return useQuery({
    enabled: !!dataSourceName && !!schemaName,
    queryKey: ['tables-schema', schemaName, dataSourceName],
    queryFn: async () => {
      try {
        // Will always fetch all tables on schema
        const runSql = dataSource?.getFetchTablesListQuery({
          schemas: [schemaName],
        });
        const url = Endpoints.query;
        const query = getRunSqlQuery(runSql, dataSourceName, false, true);
        const response = await httpClient.post(url, {
          type: 'bulk',
          source: dataSourceName,
          args: [query],
        });

        return JSON.parse(response.data?.[0]?.result?.[1]?.[0]);
      } catch (err: any) {
        throw new Error(err);
      }
    },
    refetchOnWindowFocus: false,
  });
};
