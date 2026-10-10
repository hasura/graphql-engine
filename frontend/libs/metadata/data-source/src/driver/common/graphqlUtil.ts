import { SourceCustomization } from '@hasura/shared/types';
import { formatGraphQL } from '@hasura/shared/utils';

interface GetFullQueryName {
  tableName: string;
  schema: string;
  tableConfiguration: any;
  dataSourceCustomization: SourceCustomization;
  defaultSchema?: string;
  operation: keyof Omit<
    any,
    'select_by_pk' | 'insert_one' | 'update_by_pk' | 'delete_by_pk'
  >;
}

export const getFullQueryNameBase =
  (defaultSchema: string) =>
  ({
    tableName,
    schema,
    tableConfiguration,
    dataSourceCustomization,
    operation,
  }: GetFullQueryName): string => {
    const prefix = dataSourceCustomization?.root_fields?.prefix ?? '';
    const suffix = dataSourceCustomization?.root_fields?.suffix ?? '';

    const customRootFields = tableConfiguration?.custom_root_fields ?? {};
    const customRootField = customRootFields[operation];

    const withUpdate =
      operation === 'update' && !customRootFields?.update ? 'update_' : '';
    const withSchema =
      schema === defaultSchema || tableConfiguration?.custom_name
        ? ''
        : `${schema}_`;
    const withAgg =
      operation === 'select_aggregate' && !customRootFields?.select_aggregate
        ? `_aggregate`
        : '';
    const withDelete =
      operation === 'delete' && !customRootFields?.delete ? 'delete_' : '';
    const withInsert =
      operation === 'insert' && !customRootFields?.insert ? 'insert_' : '';

    const trackedTableName =
      customRootField || tableConfiguration?.custom_name || tableName;

    return `${prefix}${withDelete}${withUpdate}${withInsert}${withSchema}${trackedTableName}${withAgg}${suffix}`;
  };

type GetQueryWithNamespaceArgs = {
  queryName: string;
  namespace: string;
  innerQuery: string;
};

export const getQueryWithNamespace = ({
  queryName,
  namespace,
  innerQuery,
}: GetQueryWithNamespaceArgs) => {
  return formatGraphQL(`${queryName}
  {
    ${namespace ? `${namespace} {` : ''}

    ${innerQuery}

    ${namespace ? `}` : ''}
  }`);
};
