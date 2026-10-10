import { DeleteRowArgs, DeleteRowProps } from '../../types';
import { getQueryWithNamespace } from '../graphqlUtil';
import { GraphQLRequestInput } from '@hasura/shared/types';
import { runGraphQL } from '@hasura/metadata/api';
import { extractTableInfo } from '@hasura/shared/utils';
import { transformWhereBoolExp } from '../validation';

/**
 * The common deleteRows function uses GraphQL to delete rows.
 */
export const deleteRowsCurry =
  (defaultSchema: string) =>
  async ({ args, fetchJson, endpoints }: DeleteRowProps): Promise<number> => {
    const { rootFieldName, ...body } = buildDeleteRequestBody(
      args,
      defaultSchema,
    );

    const result = await runGraphQL({
      url: endpoints.graphQLUrl,
      fetchJson,
      body,
    });

    if (
      result &&
      typeof result === 'object' &&
      rootFieldName in result &&
      result[rootFieldName] &&
      'affected_rows' in result[rootFieldName]
    ) {
      return result[rootFieldName].affected_rows;
    }

    return 0;
  };

const buildDeleteRequestBody = (data: DeleteRowArgs, defaultSchema: string) => {
  const tableInfo = extractTableInfo(data.table);
  const whereClause = transformWhereBoolExp(data.where, data.columns, true);

  const prefix = data.source.customization?.root_fields?.prefix ?? '';
  const suffix = data.source.customization?.root_fields?.suffix ?? '';
  const sourceConfig = (
    typeof data.source.configuration === 'object'
      ? data.source.configuration
      : {}
  ) as Record<string, any>;
  const customRootField = sourceConfig?.custom_root_fields?.update ?? '';
  const withDelete = !customRootField ? 'delete_' : '';
  const withSchema =
    !tableInfo ||
    tableInfo?.schema === defaultSchema ||
    ('custom_name' in sourceConfig && sourceConfig.custom_name)
      ? ''
      : `${tableInfo.schema}_`;
  const trackedTableName =
    customRootField || sourceConfig?.custom_name || tableInfo?.name;
  const rootFieldName = `${prefix}${withDelete}${withSchema}${trackedTableName}${suffix}`;
  // FIXME: naming convention.
  const whereTypeName = `${withSchema}${trackedTableName}_bool_exp`;

  const namespace = data.source.customization?.root_fields?.namespace ?? '';
  const innerQuery = `${rootFieldName}(where: $where, _set: $set) { affected_rows } }`;
  const operationName = 'DeleteRows';

  const query = getQueryWithNamespace({
    queryName: `mutation ${operationName}($where: ${whereTypeName})`,
    namespace,
    innerQuery,
  });

  return {
    rootFieldName,
    operationName,
    query,
    variables: {
      where: whereClause,
    },
  } as GraphQLRequestInput & { rootFieldName: string };
};
