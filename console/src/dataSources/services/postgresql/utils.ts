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

export function getTableRowRequest(
  tables: Tables,
  headers: Headers,
  isExport = false
) {
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
