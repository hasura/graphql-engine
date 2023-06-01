import { useQuery } from 'react-query';

import {
  DataSource,
  exportMetadata,
  Operator,
  TableColumn,
} from '../../../../../DataSource';
import { useHttpClient } from '../../../../../Network';

import { createDefaultValues } from './createDefaultValues';
import { createFormData } from './createFormData';
import { MetadataDataSource } from '../../../../../../metadata/types';

export type Args = {
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: 'select' | 'insert' | 'update' | 'delete';
  tableColumns: TableColumn[];
  metadataSource: MetadataDataSource | undefined;
};

export type ReturnValue = {
  formData: ReturnType<typeof createFormData> | undefined;
  defaultValues: ReturnType<typeof createDefaultValues> | undefined;
};

/**
 *
 * creates data for displaying in the form e.g. column names, roles etc.
 * creates default values for form i.e. existing permissions from metadata
 */
export const useFormData = ({
  dataSourceName,
  table,
  roleName,
  queryType,
  tableColumns = [],
  metadataSource,
}: Args) => {
  const httpClient = useHttpClient();
  return useQuery<ReturnValue, Error>({
    queryKey: [
      dataSourceName,
      'permissionFormData',
      JSON.stringify(table),
      roleName,
      tableColumns,
      queryType,
    ],
    queryFn: async () => {
      if (tableColumns.length === 0)
        return { formData: undefined, defaultValues: undefined };
      const metadata = await exportMetadata({ httpClient });

      const defaultQueryRoot = await DataSource(httpClient).getDefaultQueryRoot(
        {
          dataSourceName,
          table,
        }
      );

      const supportedOperators = (await DataSource(
        httpClient
      ).getSupportedOperators({
        dataSourceName,
      })) as Operator[];

      const defaultValues = {
        ...createDefaultValues({
          queryType,
          roleName,
          dataSourceName,
          metadata,
          table,
          tableColumns,
          defaultQueryRoot,
          metadataSource,
          supportedOperators: supportedOperators ?? [],
        }),
      };

      const formData = createFormData({
        dataSourceName,
        table,
        metadata,
        tableColumns,
        trackedTables: metadataSource?.tables,
        metadataSource,
      });

      return { formData, defaultValues };
    },
    refetchOnWindowFocus: false,
  });
};
