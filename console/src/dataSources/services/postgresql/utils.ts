import { dataSource, generateTableDef } from '../..';
import {
  getRunSqlQuery,
  getSelectQuery,
  WhereClause,
} from '../../../components/Common/utils/v1QueryUtils';
import Endpoints, { globalCookiePolicy } from '../../../Endpoints';
import requestAction from '../../../utils/requestAction';
import {
  generateInsertRequestType,
  GenerateBulkDeleteRowRequest,
  GenerateDeleteRowRequest,
} from '../../types';
import { ReduxState } from './../../../types';
import { getStatementTimeoutSql } from './sqlUtils';
import { QualifiedTable, TableConfig } from '../../../metadata/types';

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
        currentDataSource,
        false,
        true
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

const getInsertRequestBody = ({
  source,
  tableDef,
  insertObject,
  returning,
}: Parameters<
  generateInsertRequestType['getInsertRequestBody']
>[0]): ReturnType<generateInsertRequestType['getInsertRequestBody']> => {
  return {
    type: 'insert',
    args: {
      source,
      table: tableDef,
      objects: [insertObject],
      returning,
    },
  };
};

type processInsertDataParameter = Parameters<
  generateInsertRequestType['processInsertData']
>;

const processInsertData = (result: processInsertDataParameter[0]) => {
  result = result as Record<string, Record<string, any>>;
  return {
    affectedRows: result?.affected_rows,
    returnedFields: result?.returning[0],
  };
};

export const generateInsertRequest = () => ({
  endpoint: Endpoints.query,
  processInsertData,
  getInsertRequestBody,
});

export const getRowsCountRequestBody = ({
  tables,
}: {
  tables: Tables;
  tableConfiguration: TableConfig;
}) => {
  const {
    currentTable: originalTable,
    currentSchema,
    view,
    currentDataSource,
  } = tables;
  const selectQuery = getSelectQuery(
    'count',
    generateTableDef(originalTable, currentSchema),
    view.query.columns,
    view.query.where,
    view.query.offset,
    view.query.limit,
    view.query.order_by,
    currentDataSource
  );

  const queries = [
    getRunSqlQuery(getStatementTimeoutSql(2), currentDataSource, false, true),
    selectQuery,
  ];

  return {
    type: 'bulk',
    source: currentDataSource,
    args: queries,
  };
};

export const processCount = ({
  data,
}: {
  data: any;
  currentSchema: string;
  originalTable: string;
  tableConfiguration: TableConfig;
}): number => {
  return data[1].count;
};

export const generateRowsCountRequest = () => ({
  getRowsCountRequestBody,
  endpoint: Endpoints.query,
  processCount,
});

const getEditRowRequestBody = (data: {
  source: string;
  tableDef: QualifiedTable;
  set: Record<string, any>;
  where: Record<string, any>;
  defaultArray: any[];
}) => {
  return {
    type: 'update',
    args: {
      source: data.source,
      table: data.tableDef,
      $set: data.set,
      $default: data.defaultArray,
      where: data.where,
    },
  };
};

const processEditData = ({
  data,
}: {
  tableDef: QualifiedTable;
  data: any;
}): number => {
  return data.affected_rows;
};

export const generateEditRowRequest = () => ({
  endpoint: Endpoints.query,
  processEditData,
  getEditRowRequestBody,
});

const getDeleteRowRequestBody = ({
  pkClause,
  tableName,
  schemaName,
  source,
}: {
  pkClause: WhereClause;
  tableName: string;
  schemaName: string;
  source: string;
}) => {
  return {
    type: 'delete',
    args: {
      source,
      table: {
        name: tableName,
        schema: schemaName,
      },
      where: pkClause,
    },
  };
};

const processDeleteRowData = (data: any) => {
  return data.affected_rows;
};

export const generateDeleteRowRequest = (): GenerateDeleteRowRequest => ({
  endpoint: Endpoints.query,
  getDeleteRowRequestBody,
  processDeleteRowData,
});

const getBulkDeleteRowRequestBody = ({
  pkClauses,
  tableName,
  schemaName,
  source,
}: {
  pkClauses: WhereClause[];
  tableName: string;
  schemaName: string;
  source: string;
}) => {
  return {
    type: 'bulk',
    source,
    args: pkClauses.map(pkClause =>
      getDeleteRowRequestBody({ pkClause, tableName, schemaName, source })
    ),
  };
};

const processBulkDeleteRowData = (data: any) =>
  data.reduce((acc: any, d: any) => acc + d.affected_rows, 0);

export const generateBulkDeleteRowRequest = (): GenerateBulkDeleteRowRequest => ({
  endpoint: Endpoints.query,
  getBulkDeleteRowRequestBody,
  processBulkDeleteRowData,
});
