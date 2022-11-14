import React, { useEffect, useMemo } from 'react';
import { useAppDispatch, useAppSelector } from '@/store';
import { getManualEventsTriggers } from '@/metadata/selector';
import { runFilterQuery } from '@/features/BrowseRows';

import { setTable } from '../DataActions';
import ViewRows from './ViewRows';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import { Table } from '../../../../dataSources/types';
import { dataSource, isFeatureSupported } from '../../../../dataSources';
import { exists } from '../../../Common/utils/jsUtils';
import TableHeader from '../TableCommon/TableHeader';
import FeatureDisabled from '../FeatureDisabled';
import { getPersistedPageSize } from './tableUtils';
import { vSetDefaults } from './ViewActions';
import { adaptFormValuesToQuery } from '../../../../features/BrowseRows/components/RunQuery/LegacyRunQueryContainer/LegacyRunQueryContainer.utils';

type TableBrowseRowsContainerProps = {
  params: {
    schema: string;
    source: string;
    table: string;
  };
};
export const getUrlQueryParams = (): {
  filters: any['filter'];
  sorts: any['sort'];
} => {
  const params = new URLSearchParams(window.location.search);
  const filters = params.getAll('filter') ?? [];
  const sorts = params.getAll('sort') ?? [];

  return {
    filters: filters.map(filter => {
      const [column, operator, value] = filter.split(';');
      return {
        column,
        operator,
        value,
      };
    }),
    sorts: sorts.map(filter => {
      const [column, type] = filter.split(';');
      return {
        column,
        type: type as any['sort'][0]['type'],
      };
    }),
  };
};
export const TableBrowseRowsContainer = (
  props: TableBrowseRowsContainerProps
) => {
  const {
    table: tableName,
    source: sourceName,
    schema: schemaName,
  } = props.params;
  const dispatch = useAppDispatch();

  const schemas = useAppSelector<Table[]>(store => store.tables.allSchemas);
  const {
    query,
    rows,
    lastError,
    lastSuccess,
    activePath,
    count,
    expandedRow,
    curFilter,
    ongoingRequest,
    estimatedCount,
    isCountEstimated,
    isProgressing,
  } = useAppSelector(store => store.tables.view);

  const migrationMode = useAppSelector(store => store.main.migrationMode);
  const readOnlyMode = useAppSelector(store => store.main.readOnlyMode);
  const manualTriggers = useAppSelector(store =>
    getManualEventsTriggers(store)
  );

  const { table, isTableView } = useMemo(() => {
    const currentTable = schemas.find(
      s => s.table_name === tableName && s.table_schema === schemaName
    );

    return {
      table: currentTable,
      isTableView: currentTable && !dataSource.isTable(currentTable),
    };
  }, [tableName]);

  useEffect(() => {
    const getInitialData = () => {
      const tableSchema = schemas.find(
        s => s.table_name === tableName && s.table_schema === schemaName
      );
      if (!tableSchema) return;

      if (!isFeatureSupported('tables.browse.enabled')) {
        dispatch(setTable(tableName));
      }
      dispatch(setTable(tableName));
      const queryParams = getUrlQueryParams();
      const limit = getPersistedPageSize();

      dispatch(vSetDefaults(limit));

      const userQuery = adaptFormValuesToQuery(
        { filter: queryParams.filters, sort: queryParams.sorts },
        tableSchema.columns.map(column => ({
          name: column.column_name,
          dataType: column.data_type,
        }))
      );

      dispatch(
        runFilterQuery({
          tableSchema,
          whereAnd: userQuery.where.$and,
          orderBy: userQuery.order_by,
          limit: curFilter.limit,
          offset: curFilter.offset,
        })
      );
    };

    getInitialData();

    return () => {
      dispatch(vSetDefaults());
    };
  }, [tableName]);
  if (!isFeatureSupported('tables.browse.enabled')) {
    return (
      <FeatureDisabled
        tab="browse"
        tableName={tableName}
        schemaName={schemaName}
      />
    );
  }
  const shouldUseCustomPagination = isFeatureSupported(
    'tables.browse.customPagination'
  );
  const tableCount = exists(count) ? count : estimatedCount;
  const shouldHidePagination = !exists(count) && !estimatedCount;

  return (
    <RightContainer>
      <TableHeader
        count={isCountEstimated ? estimatedCount : count}
        isCountEstimated={isCountEstimated}
        dispatch={dispatch}
        table={table}
        source={sourceName}
        tabName="browse"
        migrationMode={migrationMode}
        readOnlyMode={readOnlyMode}
      />
      <ViewRows
        curTableName={tableName}
        currentSchema={schemaName}
        dispatch={dispatch}
        readOnlyMode={readOnlyMode}
        parentTableName={null}
        curQuery={query}
        curFilter={curFilter}
        curRows={rows}
        isView={isTableView}
        activePath={activePath}
        currentSource={sourceName}
        ongoingRequest={ongoingRequest}
        lastError={lastError}
        lastSuccess={lastSuccess}
        schemas={schemas}
        curDepth={0}
        count={tableCount}
        shouldHidePagination={shouldHidePagination}
        expandedRow={expandedRow}
        manualTriggers={manualTriggers}
        useCustomPagination={shouldUseCustomPagination}
        isProgressing={isProgressing}
      />
    </RightContainer>
  );
};
