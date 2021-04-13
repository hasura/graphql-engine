import { dataSource, generateTableDef } from '../..';
import {
  getRunSqlQuery,
  getSelectQuery,
} from '../../../components/Common/utils/v1QueryUtils';
import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import { ReduxState } from './../../../types';

type Tables = ReduxState['tables'];
type Headers = Tables['dataHeaders'];

export function getTableRowRequest({
  tables,
  headers,
  isExport = false,
}: {
  tables: Tables;
  headers: Headers;
  isExport?: boolean;
}) {
  const {
    currentTable: originalTable,
    currentSchema,
    currentDataSource,
    view,
  } = tables;
  const limit = isExport ? null : view.query.limit;
  const offset = isExport ? null : view.query.offset;
  const requestBody = {
    type: 'bulk',
    source: currentDataSource,
    args: [
      getSelectQuery(
        'select',
        generateTableDef(originalTable, currentSchema),
        view.query.columns,
        view.query.where,
        offset,
        limit,
        view.query.order_by,
        currentDataSource
      ),
      getRunSqlQuery(
        dataSource.getEstimateCountQuery(currentSchema, originalTable),
        currentDataSource
      ),
    ],
  };
  const options: RequestInit = {
    method: 'POST',
    body: JSON.stringify(requestBody),
    headers,
    credentials: globalCookiePolicy,
  };

  return requestAction(Endpoints.query, options);
}

const getTableRowRequestBody = ({
  tables,
  isExport = false,
}: {
  tables: Tables;
  isExport?: boolean;
}) => {
  const {
    currentTable: originalTable,
    currentSchema,
    currentDataSource,
    view,
  } = tables;
  const limit = isExport ? null : view.query.limit;
  const offset = isExport ? null : view.query.offset;
  const requestBody = {
    type: 'bulk',
    source: currentDataSource,
    args: [
      getSelectQuery(
        'select',
        generateTableDef(originalTable, currentSchema),
        view.query.columns,
        view.query.where,
        offset,
        limit,
        view.query.order_by,
        currentDataSource
      ),
      getRunSqlQuery(
        dataSource.getEstimateCountQuery(currentSchema, originalTable),
        currentDataSource
      ),
    ],
  };
  return requestBody;
};

const processTableRowData = (data: any) => {
  let estimatedCount =
    data.length > 1 && data[0].result > 1 && data.result[1].length
      ? data[1].result[1][0]
      : null;
  estimatedCount =
    estimatedCount !== null ? parseInt(data[1]?.result[1][0], 10) : null;
  return { rows: data[0], estimatedCount };
};

export const generateTableRowRequest = () => {
  return {
    endpoint: Endpoints.query,
    getTableRowRequestBody,
    processTableRowData,
  };
};
