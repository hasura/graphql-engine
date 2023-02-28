import { Table, TableRow, TableHeader } from '../../components/Common/Table';
import { currentDriver, Driver, setDriver } from '../../dataSources';
import { TableColumn } from '../../dataSources/types';
import { QualifiedTable } from '../../metadata/types';

import { useAppDispatch, useAppSelector } from '../../storeHooks';
import React from 'react';
import { useSingleTable } from '..';

export const SingleTable = ({
  driver,
  currentDatasource,
  table,
}: {
  driver: Driver;
  currentDatasource: string;
  table: QualifiedTable;
}) => {
  const dispatch = useAppDispatch();
  React.useEffect(() => {
    if (currentDatasource) {
      dispatch({
        type: 'Data/UPDATE_CURRENT_DATA_SOURCE',
        source: currentDatasource,
      });
    }
    if (driver) {
      setDriver(driver);
    }
  }, [currentDatasource, dispatch, driver]);

  const source = useAppSelector(s => s.tables.currentDataSource);

  const {
    data: primaryKey,
    isLoading,
    isSuccess,
    isError,
    isIdle,
    error,
  } = useSingleTable({ table, source, driver: currentDriver }, { retry: 0 });
  const keys = React.useMemo(() => Object.keys(primaryKey ?? {}), [primaryKey]);
  const entries = React.useMemo(
    () => Object.values(primaryKey ?? {}),
    [primaryKey]
  );

  if (isError) {
    return <p className="text-red-500">Error: {error?.message}</p>;
  }

  if (isLoading || isIdle) {
    return <p>Loading...</p>;
  }

  return (
    <div>
      <h1 className="prose-xl">useSingleTable</h1>
      <p className="prose-lg mb-8">
        <b>
          {table.schema}.{table.name}
        </b>{' '}
        table
      </p>
      {isSuccess && (
        <Table rowCount={1} columnCount={keys?.length ?? 0}>
          <TableHeader headers={keys.length ? keys : ['EmptyValue']} />
          <TableRow
            entries={entries.length ? entries : ['null']}
            index=""
            readonly
            renderCol={({ data, index }) =>
              index === 4
                ? data?.map((d: TableColumn) => d.column_name).join(', ')
                : typeof data === 'string'
                ? data
                : JSON.stringify(data)
            }
          />
        </Table>
      )}
    </div>
  );
};
