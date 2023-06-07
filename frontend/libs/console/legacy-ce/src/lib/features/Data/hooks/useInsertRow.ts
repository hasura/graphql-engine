import { DataSource } from '../../DataSource';
import { generateGraphQLInsertMutation } from '../../GraphQLUtils';
import { areTablesEqual, useMetadata } from '../../hasura-metadata-api';
import { Table } from '../../hasura-metadata-types';
import { useHttpClient } from '../../Network';
import { useGraphQLMutation } from './useGraphQLMutation';

export type FormData = Record<
  string,
  | {
      option: 'value';
      value: unknown;
    }
  | { option: 'default' }
  | { option: 'null' }
>;

type UseInsertRowProps = {
  table: Table;
  dataSourceName: string;
  onSuccess?: () => void;
  onError?: (err: Error) => void;
};

export const useInsertRow = ({
  table,
  dataSourceName,
  onSuccess,
  onError,
}: UseInsertRowProps) => {
  const { data } = useMetadata(m => ({
    sourceCustomization: m.metadata.sources?.find(
      s => s.name === dataSourceName
    )?.customization,
    tableCustomization: m.metadata.sources
      ?.find(s => s.name === dataSourceName)
      ?.tables.find(t => areTablesEqual(t.table, table))?.configuration,
  }));

  const httpClient = useHttpClient();

  const { mutate, ...rest } = useGraphQLMutation({
    operationName: 'insertRow',
    onSuccess,
    onError,
  });

  const insertRow = async (values: FormData) => {
    const defaultQueryRoot = await DataSource(httpClient).getDefaultQueryRoot({
      dataSourceName,
      table,
    });

    const mutationObject = Object.entries(values).reduce((acc, val) => {
      const [columnName, body] = val;

      if (body.option === 'value')
        return {
          ...acc,
          [columnName]: body.value,
        };

      return acc;
    }, {});

    const gqlQuery = generateGraphQLInsertMutation({
      defaultQueryRoot,
      tableCustomization: data?.tableCustomization,
      sourceCustomization: data?.sourceCustomization,
      objects: [mutationObject],
      mutationName: 'insertRow',
    });

    return mutate(gqlQuery);
  };

  return { insertRow, ...rest };
};
