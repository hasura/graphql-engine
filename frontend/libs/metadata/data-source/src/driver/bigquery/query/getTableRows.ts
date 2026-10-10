import { generateGraphQLSelectQuery } from '@hasura/shared/utils';
import get from 'lodash/get';
import { BigQueryTable } from '../types';
import { transformGraphqlResponse } from '../../common/utils';
import { GetTableRowsProps, TableRow } from '../../types';
import { exportMetadata, runGraphQL } from '@hasura/metadata/api';

export const getTableRows = async ({
  table,
  dataSourceName,
  columns,
  options,
  fetchJson,
  endpoints,
}: GetTableRowsProps): Promise<TableRow[]> => {
  const { name, dataset } = table as BigQueryTable;

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
    const metadataTableDef = t as BigQueryTable;
    return (
      metadataTableDef.name === name && metadataTableDef.dataset === dataset
    );
  });

  if (!trackedTable)
    throw new Error('getTableRows: trackedTable not found in metadata');

  const defaultQueryRoot = `${dataset}_${name}`;

  const { query, resultPath } = await generateGraphQLSelectQuery({
    operationName: 'TableRows',
    defaultQueryRoot,
    columns: columns.map((col) => col.name),
    tableCustomization: trackedTable.configuration,
    sourceCustomization: source.customization,
    options,
  });

  const graphqlResponse = await runGraphQL({
    fetchJson,
    url: endpoints.graphQLUrl,
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
