import { generateGraphQLSelectQuery } from '../../../GraphQLUtils';
import get from 'lodash/get';
import { GDCTable } from '..';
import { exportMetadata, runGraphQL } from '../../api';
import { transformGraphqlResponse } from '../../common/utils';
import { GetTableRowsProps, TableRow } from '../../types';

export const getTableRows = async ({
  table,
  dataSourceName,
  httpClient,
  columns,
  options,
}: GetTableRowsProps): Promise<TableRow[]> => {
  const source = (await exportMetadata({ httpClient })).metadata.sources.find(
    ({ name: sourceName }) => sourceName === dataSourceName
  );

  /**
   * If I can't find the source in the metadata, then there is something inconsistent on the server.
   */
  if (!source) throw new Error('getTableRows: source not found in metadata');

  // TODO: I think we can make it better, more generic if we relegate the table comparison thingy to a util function.
  const trackedTable = source.tables.find(({ table: t }) => {
    const metadataTableDef = t as GDCTable;
    return metadataTableDef.join() === (table as GDCTable).join();
  });

  if (!trackedTable)
    throw new Error('getTableRows: trackedTable not found in metadata');

  const defaultQueryRoot = (table as GDCTable).join('_');

  const { query, resultPath } = await generateGraphQLSelectQuery({
    operationName: 'TableRows',
    defaultQueryRoot,
    columns,
    tableCustomization: trackedTable.configuration,
    sourceCustomization: source.customization,
    options,
  });

  const graphqlResponse = await runGraphQL({
    operationName: 'TableRows',
    query,
    httpClient,
  });

  if ('errors' in graphqlResponse)
    throw new Error('Unable to fetch GraphQL response');

  const result = transformGraphqlResponse({
    data: get(graphqlResponse.data, resultPath),
    tableCustomization: trackedTable.configuration,
    sourceCustomization: source.customization,
    columns,
  });

  return result;
};
