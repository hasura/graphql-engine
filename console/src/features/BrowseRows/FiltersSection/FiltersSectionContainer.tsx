import { dataSource } from '@/dataSources';
import { useAppDispatch, useAppSelector } from '@/store';
import React, { useEffect, useState } from 'react';
import {
  downloadObjectAsCsvFile,
  downloadObjectAsJsonFile,
  getCurrTimeForFileName,
} from '@/components/Common/utils/jsUtils';
import { vMakeExportRequest } from '../../../components/Services/Data/TableBrowseRows/ViewActions';
import { setOffset } from '../../../components/Services/Data/TableBrowseRows/FilterActions';
import { OperatorItem } from './FilterRow';
import {
  adaptFormValuesToQuery,
  filterValidUserQuery,
  getColumns,
  runFilterQuery,
  setUrlParams,
} from './FiltersSectionContainer.utils';
import { FiltersSection, sortOptions } from './FiltersSection';
import { FiltersAndSortFormValues, UserQuery } from './types';
import { useTableSchema } from './hooks/useTableSchema';
import type { BrowseRowsTable } from './hooks/useTableSchema.utils';
import { useTableColumns } from './hooks/useTableColumns';
import { useTableName } from './hooks/useTableName';

type FilterSectionContainerProps = {
  onRunQuery: (userQuery: UserQuery) => void | null;
  dataSourceName: string;
  table: BrowseRowsTable;
};

const replaceAllDotsWithUnderscore = (text: string) => text.replace(/\./g, '_');
const getFileName = (tableName: string) => {
  const replacedTableName = replaceAllDotsWithUnderscore(tableName);
  const currentTime = getCurrTimeForFileName();
  return `export_${replacedTableName}_${currentTime}`;
};

export const FilterSectionContainer = ({
  onRunQuery,
  dataSourceName,
  table,
}: FilterSectionContainerProps) => {
  const dispatch = useAppDispatch();
  const query = useAppSelector(state => state.tables.view.query);
  const curFilter = useAppSelector(state => state.tables.view.curFilter);
  const limit = curFilter.limit;
  const offset = curFilter.offset;

  const tableSchema = useTableSchema(table);

  const tableColumns = useTableColumns({ dataSourceName, table });

  const columns = getColumns(query.columns);

  const [operators, setOperators] = useState<OperatorItem[]>([]);
  useEffect(() => {
    const operatorItems: OperatorItem[] = dataSource.operators.map(op => {
      return {
        label: `[${op.graphqlOp}] ${op.name}`,
        value: op.value,
        defaultValue: op?.defaultValue,
      };
    });
    setOperators(operatorItems);
  }, []);

  const onSubmit = (userQuery: UserQuery) => {
    if (!tableSchema) {
      console.error('tableSchema is not defined', tableSchema);
      return;
    }

    dispatch(setOffset(0));
    setUrlParams(userQuery.where.$and, userQuery.order_by);
    dispatch(
      runFilterQuery({
        tableSchema,
        whereAnd: userQuery.where.$and,
        orderBy: userQuery.order_by,
        limit,
        offset,
      })
    );
  };

  const tableName = useTableName({ dataSourceName, table });
  const onExportData = (
    type: 'CSV' | 'JSON',
    formValues: FiltersAndSortFormValues
  ) => {
    const userQuery = filterValidUserQuery(
      adaptFormValuesToQuery(formValues, tableColumns)
    );

    const fileName = getFileName(tableName);
    dispatch({ type: 'ViewTable/V_SET_QUERY_OPTS', queryStuff: userQuery });
    dispatch(vMakeExportRequest()).then((rows: Record<string, unknown>[]) => {
      if (!rows || rows.length === 0) {
        return;
      }

      switch (type) {
        case 'JSON':
          downloadObjectAsJsonFile(fileName, rows);
          break;
        case 'CSV':
          downloadObjectAsCsvFile(fileName, rows);
          break;
        default:
      }
    });
  };

  return (
    <FiltersSection
      columns={columns}
      operators={operators}
      orders={sortOptions}
      onExport={onExportData}
      onSubmit={(values: FiltersAndSortFormValues) => {
        const userQuery = filterValidUserQuery(
          adaptFormValuesToQuery(values, tableColumns)
        );
        if (onRunQuery) {
          onRunQuery(userQuery);
        }
        onSubmit(userQuery);
      }}
    />
  );
};
