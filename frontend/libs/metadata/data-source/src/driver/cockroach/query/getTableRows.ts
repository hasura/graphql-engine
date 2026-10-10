import { generateGraphQLSelectQuery } from '@hasura/shared/utils';
import get from 'lodash/get';
import { CockroachDBTable } from '../types';
import { transformGraphqlResponse } from '../../common/utils';
import { GetTableRowsProps, TableRow } from '../../types';
import { exportMetadata, runGraphQL } from '@hasura/metadata/api';

export const getTableRows = async ({
  table,
  dataSourceName,
  columns,
  options,
  endpoints,
  fetchJson,
}: GetTableRowsProps): Promise<TableRow[]> => {
  const { name, schema } = table as CockroachDBTable;

  const source = (
    await exportMetadata({
      fetchJson,
      url: endpoints.metadata,
    })
  ).metadata.sources.find(
    ({ name: sourceName }) => sourceName === dataSourceName,
  );

  /**
   * If I can't find the source in the metadata, then there is something inconsistent on the server.
   */
  if (!source) throw new Error('getTableRows: source not found in metadata');

  // TODO: I think we can make it better, more generic if we relegate the table comparison thingy to a util function.
  const trackedTable = source.tables.find(({ table: t }) => {
    const metadataTableDef = t as CockroachDBTable;
    return metadataTableDef.name === name && metadataTableDef.schema === schema;
  });

  if (!trackedTable)
    throw new Error('getTableRows: trackedTable not found in metadata');

  const defaultQueryRoot = schema === 'dbo' ? name : `${schema}_${name}`;

  const { query, resultPath } = await generateGraphQLSelectQuery({
    operationName: 'TableRows',
    defaultQueryRoot,
    columns: columns.map((col) => col.name),
    tableCustomization: trackedTable.configuration,
    sourceCustomization: source.customization,
    options,
  });

  const graphqlResponse = await runGraphQL({
    url: endpoints.graphQLUrl,
    fetchJson,
    body: {
      operationName: 'TableRows',
      query,
    },
  });

  const result = transformGraphqlResponse({
    data: get(graphqlResponse.data, resultPath) ?? [],
    tableCustomization: trackedTable.configuration,
  });

  return result;
};
