import { useQuery } from 'react-query';
import { runMetadataQuery } from '../../DataSource';
import { GetTableInfoResponse } from '../../DataSource/gdc/introspection';
import { Table } from '../../hasura-metadata-types';
import { useHttpClient } from '../../Network';

type UseTableInfoArg = {
  dataSourceName: string;
  table: Table;
  isEnabled?: boolean;
};

export const useTableInfo = ({
  dataSourceName,
  table,
  isEnabled = true,
}: UseTableInfoArg) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: [dataSourceName, table, 'table-info'],
    queryFn: async () =>
      await runMetadataQuery<GetTableInfoResponse>({
        httpClient,
        body: {
          type: 'get_table_info',
          args: {
            source: dataSourceName,
            table,
          },
        },
      }),
    enabled: isEnabled,
  });
};
