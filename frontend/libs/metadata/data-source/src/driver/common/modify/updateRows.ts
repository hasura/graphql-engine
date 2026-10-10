import {
  UpdateRowArgs,
  UpdateRowProps,
  ValidateInputRowValuesFunction,
} from '../../types';
import { getQueryWithNamespace } from '../graphqlUtil';
import { GraphQLRequestInput } from '@hasura/shared/types';
import { runGraphQL } from '@hasura/metadata/api';
import { extractTableInfo } from '@hasura/shared/utils';
import { transformWhereBoolExp } from '../validation';

/**
 * The common updateRows function uses GraphQL to update rows.
 */
export const updateRowsCurry =
  (validate: ValidateInputRowValuesFunction, defaultSchema: string) =>
  async ({ args, fetchJson, endpoints }: UpdateRowProps): Promise<number> => {
    const { rootFieldName, ...body } = buildUpdateRequestBody(
      args,
      validate,
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

const buildUpdateRequestBody = (
  data: UpdateRowArgs,
  validate: ValidateInputRowValuesFunction,
  defaultSchema: string,
) => {
  const tableInfo = extractTableInfo(data.table);
  const whereClause = transformWhereBoolExp(data.where, data.columns, true);

  const setClause = validate({
    columns: data.columns,
    graphqlMode: true,
    values: data.set,
  });

  const prefix = data.source.customization?.root_fields?.prefix ?? '';
  const suffix = data.source.customization?.root_fields?.suffix ?? '';
  const sourceConfig = (
    typeof data.source.configuration === 'object'
      ? data.source.configuration
      : {}
  ) as Record<string, any>;
  const customRootField = sourceConfig?.custom_root_fields?.update ?? '';
  const withUpdate = !customRootField ? 'update_' : '';
  const withSchema =
    !tableInfo ||
    tableInfo?.schema === defaultSchema ||
    ('custom_name' in sourceConfig && sourceConfig.custom_name)
      ? ''
      : `${tableInfo.schema}_`;
  const trackedTableName =
    customRootField || sourceConfig?.custom_name || tableInfo?.name;
  const rootFieldName = `${prefix}${withUpdate}${withSchema}${trackedTableName}${suffix}`;
  // FIXME: naming convention.
  const whereTypeName = `${withSchema}${trackedTableName}_bool_exp`;
  const setTypeName = `${withSchema}${trackedTableName}_set_input!`;

  const namespace = data.source.customization?.root_fields?.namespace ?? '';
  const innerQuery = `${rootFieldName}(where: $where, _set: $set) { affected_rows } }`;

  const query = getQueryWithNamespace({
    queryName: `mutation UpdateRows($where: ${whereTypeName}, $set: ${setTypeName})`,
    namespace,
    innerQuery,
  });

  return {
    rootFieldName,
    operationName: 'EditRow',
    query,
    variables: {
      where: whereClause,
      _set: setClause,
    },
  } as GraphQLRequestInput & { rootFieldName: string };
};
