import {
  InsertRowArgs,
  InsertRowProps,
  ValidateInputRowValuesFunction,
} from '../../types';
import { getQueryWithNamespace } from '../graphqlUtil';
import { GraphQLRequestInput } from '@hasura/shared/types';
import { runGraphQL } from '@hasura/metadata/api';

import { extractTableInfo } from '@hasura/shared/utils';
/**
 * The common insertRows function uses GraphQL to update rows.
 */
export const insertRowsCurry =
  (validate: ValidateInputRowValuesFunction, defaultSchema: string) =>
  async ({ args, fetchJson, endpoints }: InsertRowProps): Promise<number> => {
    const { rootFieldName, ...body } = buildInsertRequestBody(
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

const buildInsertRequestBody = (
  data: InsertRowArgs,
  validate: ValidateInputRowValuesFunction,
  defaultSchema: string,
) => {
  const tableInfo = extractTableInfo(data.table);
  const objects = data.objects.map((obj) =>
    validate({
      columns: data.columns,
      graphqlMode: true,
      values: obj,
    }),
  );

  const prefix = data.source.customization?.root_fields?.prefix ?? '';
  const suffix = data.source.customization?.root_fields?.suffix ?? '';
  const sourceConfig = (
    typeof data.source.configuration === 'object'
      ? data.source.configuration
      : {}
  ) as Record<string, any>;
  const customRootField = sourceConfig?.custom_root_fields?.update ?? '';
  const withUpdate = !customRootField ? 'insert_' : '';
  const withSchema =
    !tableInfo ||
    tableInfo?.schema === defaultSchema ||
    ('custom_name' in sourceConfig && sourceConfig.custom_name)
      ? ''
      : `${tableInfo.schema}_`;
  const trackedTableName =
    customRootField || sourceConfig?.custom_name || tableInfo?.name;
  const rootFieldName = `${prefix}${withUpdate}${withSchema}${trackedTableName}${suffix}`;
  const objectsTypeName = `[${withSchema}${trackedTableName}_insert_input!]!`;

  const namespace = data.source.customization?.root_fields?.namespace ?? '';
  const innerQuery = `${rootFieldName}(objects: $objects) { affected_rows } }`;
  const operationName = 'InsertRow';

  const query = getQueryWithNamespace({
    queryName: `mutation ${operationName}($objects: ${objectsTypeName})`,
    namespace,
    innerQuery,
  });

  return {
    rootFieldName,
    operationName,
    query,
    variables: {
      objects,
    },
  } as GraphQLRequestInput & { rootFieldName: string };
};
