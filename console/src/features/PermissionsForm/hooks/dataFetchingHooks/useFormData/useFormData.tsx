import { useQuery } from 'react-query';
import { buildClientSchema } from 'graphql';

import {
  DataSource,
  exportMetadata,
  runIntrospectionQuery,
} from '@/features/DataSource';
import { useHttpClient } from '@/features/Network';

import { createDefaultValues } from './createDefaultValues';
import { createFormData } from './createFormData';

export type Args = {
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: 'select' | 'insert' | 'update' | 'delete';
};

type ReturnValue = {
  formData: ReturnType<typeof createFormData>;
  defaultValues: ReturnType<typeof createDefaultValues>;
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
}: Args) => {
  const httpClient = useHttpClient();
  return useQuery<ReturnValue, Error>({
    queryKey: [
      dataSourceName,
      'permissionFormData',
      JSON.stringify(table),
      roleName,
    ],
    queryFn: async () => {
      const introspectionResult = await runIntrospectionQuery({ httpClient });
      const schema = buildClientSchema(introspectionResult.data);
      const metadata = await exportMetadata({ httpClient });

      // get table columns for metadata table from db introspection
      const tableColumns = await DataSource(httpClient).getTableColumns({
        dataSourceName,
        table,
      });

      const defaultValues = createDefaultValues({
        queryType,
        roleName,
        dataSourceName,
        metadata,
        table,
        tableColumns,
        schema,
      });

      const formData = createFormData({
        dataSourceName,
        table,
        metadata,
        tableColumns,
      });

      return { formData, defaultValues };
    },
    refetchOnWindowFocus: false,
  });
};
