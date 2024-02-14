import { useQuery } from 'react-query';

import {
  DataSource,
  exportMetadata,
  Operator,
} from '../../../../../DataSource';
import { useHttpClient } from '../../../../../Network';

import { createDefaultValues } from './createDefaultValues';
import { createFormData } from './createFormData';
import { Table } from '../../../../../hasura-metadata-types';

export type Args = {
  dataSourceName: string;
  table: unknown;
  roleName: string;
  queryType: 'select' | 'insert' | 'update' | 'delete';
};

export type ReturnValue = {
  formData: ReturnType<typeof createFormData> | undefined;
  defaultValues: ReturnType<typeof createDefaultValues> | undefined;
};

export function permissionsFormKey({
  dataSourceName,
  table,
}: {
  dataSourceName: string;
  table: Table;
}) {
  return [dataSourceName, 'permissionFormData', table];
}

/**
 *
 * creates data for displaying in the form e.g. column names, roles etc.
 * creates default values for form i.e. existing permissions from metadata
 */
export const useFormData = ({ dataSourceName, table }: Args) => {
  const httpClient = useHttpClient();
  return useQuery({
    queryKey: permissionsFormKey({
      dataSourceName,
      table,
    }),
    queryFn: async () => {
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

      return { defaultQueryRoot, supportedOperators, metadata };
    },
    refetchOnWindowFocus: false,
  });
};
