import React, { useEffect, useMemo, useState } from 'react';
import { useAppDispatch, useAppSelector } from '../../../../storeHooks';
import { getManualEventsTriggers } from '../../../../metadata/selector';
import {
  adaptFormValuesToQuery,
  getFiltersAndSortFromUrlQueryParams,
  runFilterQuery,
  UserQuery,
} from '../../../../features/BrowseRows';
import useIsFirstRender from '../../../../hooks/useIsFirstRender';
import { TableColumn } from '../../../../features/DataSource';
import { vSetDefaults } from './ViewActions';
import { setTable } from '../DataActions';
import ViewRows from './ViewRows';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import { Table } from '../../../../dataSources/types';
import { dataSource, isFeatureSupported } from '../../../../dataSources';
import { exists } from '../../../Common/utils/jsUtils';
import TableHeader from '../TableCommon/TableHeader';
import FeatureDisabled from '../FeatureDisabled';
import {
  getFiltersAndSortFromLocalStorage,
  getUniqueTableKey,
  saveUserQueryToLocalStorage,
  useFiltersAndSortFormValues,
} from './Hooks/useFiltersAndSortFormValues';
import { setOffset } from './FilterActions';

type TableBrowseRowsContainerProps = {
  params: {
    schema: string;
    source: string;
    table: string;
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

  const tableSchema = schemas.find(
    x => x.table_name === tableName && x.table_schema === schemaName
  );

  const { filtersAndSort, onRunQuery, initialUserQuery } =
    useFiltersAndSortFormValues({
      sourceName,
      tableSchema,
    });

  const isFirstRender = useIsFirstRender();
  useEffect(() => {
    if (!isFirstRender) {
      return;
    }

    const getInitialData = () => {
      if (!tableSchema) return;

      dispatch(setTable(tableName));

      dispatch(vSetDefaults(curFilter.limit));

      const filterAndSortFromQueryParams =
        getFiltersAndSortFromUrlQueryParams();

      const initialUserQueryFromUrlParams = adaptFormValuesToQuery(
        filterAndSortFromQueryParams,
        (tableSchema?.columns || []).map(column => ({
          name: column.column_name,
          dataType: column.data_type as TableColumn['dataType'],
          consoleDataType: 'string',
        }))
      );

      const areFiltersAndSortsEmpty =
        filterAndSortFromQueryParams.filters.length === 0 &&
        filterAndSortFromQueryParams.sorts.length === 0;

      if (areFiltersAndSortsEmpty) {
        const localUserQuery = getFiltersAndSortFromLocalStorage({
          sourceName,
          tableSchema,
        });

        dispatch(
          runFilterQuery({
            tableSchema,
            whereAnd: localUserQuery.where.$and,
            orderBy: localUserQuery.order_by,
            limit: curFilter.limit,
            offset: curFilter.offset,
          })
        );
        return;
      }

      const uniqueTableKey = getUniqueTableKey({ sourceName, tableSchema });
      if (uniqueTableKey) {
        saveUserQueryToLocalStorage(
          uniqueTableKey,
          initialUserQueryFromUrlParams
        );
      }

      dispatch(
        runFilterQuery({
          tableSchema,
          whereAnd: initialUserQueryFromUrlParams.where.$and,
          orderBy: initialUserQueryFromUrlParams.order_by,
          limit: curFilter.limit,
          offset: curFilter.offset,
        })
      );
    };

    getInitialData();
  }, []);

  useEffect(() => {
    if (isFirstRender) {
      return;
    }
    const getInitialData = () => {
      if (!tableSchema) return;

      dispatch(setTable(tableName));
      dispatch(vSetDefaults(curFilter.limit));
      dispatch(setOffset(0));

      const localUserQuery = getFiltersAndSortFromLocalStorage({
        sourceName,
        tableSchema,
      });

      dispatch(
        runFilterQuery({
          tableSchema,
          whereAnd: localUserQuery.where.$and,
          orderBy: localUserQuery.order_by,
          limit: curFilter.limit,
          offset: 0,
        })
      );
    };

    getInitialData();
  }, [tableName]);

  const [paginationUserQuery, setPaginationUserQuery] =
    useState(initialUserQuery);

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

  const onChangePageSize = (newLimit: number) =>
    dispatch(vSetDefaults(newLimit));

  return (
    <div className="bootstrap-jail table-browse-rows-container">
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
          activePath={activePath}
          count={tableCount}
          curDepth={0}
          curFilter={curFilter}
          curQuery={query}
          currentSchema={schemaName}
          currentSource={sourceName}
          curRows={rows}
          curTableName={tableName}
          dispatch={dispatch}
          expandedRow={expandedRow}
          isProgressing={isProgressing}
          isView={isTableView}
          lastError={lastError}
          lastSuccess={lastSuccess}
          manualTriggers={manualTriggers}
          ongoingRequest={ongoingRequest}
          parentTableName={null}
          readOnlyMode={readOnlyMode}
          schemas={schemas}
          shouldHidePagination={shouldHidePagination}
          useCustomPagination={shouldUseCustomPagination}
          filtersAndSort={filtersAndSort}
          onRunQuery={(newUserQuery: UserQuery) => {
            onRunQuery(newUserQuery);
            setPaginationUserQuery(newUserQuery);
          }}
          paginationUserQuery={paginationUserQuery}
          onChangePageSize={onChangePageSize}
        />
      </RightContainer>
    </div>
  );
};
