import { Table, TableRow, TableHeader } from '../../components/Common/Table';
import { currentDriver, Driver, setDriver } from '../../dataSources';
import { TableColumn } from '../../dataSources/types';

import { useAppDispatch, useAppSelector } from '../../storeHooks';
import React from 'react';
import { useSchemaList, useDataSourceTables } from '..';

export const AllTables = ({
  driver,
  currentDatasource,
}: {
  driver: Driver;
  currentDatasource: string;
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

  const source: string = useAppSelector(s => s.tables.currentDataSource);

  const {
    data: schemas,
    isError,
    error,
  } = useSchemaList(
    {
      source,
      driver: currentDriver,
    },
    { retry: 0 }
  );
  const {
    data: tables,
    isLoading,
    isSuccess,
    isError: isError2,
    isIdle,
    error: fkError,
  } = useDataSourceTables(
    { schemas: schemas!, source, driver: currentDriver },
    {
      enabled: !!schemas,
      retry: 0,
    }
  );
  const keys = React.useMemo(() => Object.keys(tables?.[0] ?? {}), [tables]);

  if (isError || isError2) {
    return (
      <p className="text-red-500">
        Error: {error?.message || fkError?.message}
      </p>
    );
  }

  if (isLoading || isIdle) {
    return <p>Loading...</p>;
  }

  return (
    <div>
      <h1 className="prose-xl">useDataSourceTables</h1>
      <p className="prose-lg mb-8">
        Tables in <b>{schemas?.join(', ')}</b> schemas
      </p>
      {isSuccess && (
        <Table rowCount={tables?.length ?? 0} columnCount={keys?.length ?? 0}>
          <TableHeader headers={keys} />
          {tables?.map((fk, i) => {
            return (
              <TableRow
                entries={Object.values(fk)}
                index=""
                key={i}
                readonly
                renderCol={({ data, index }) =>
                  index === 4
                    ? data?.map((d: TableColumn) => d.column_name).join(', ')
                    : typeof data === 'string'
                    ? data
                    : JSON.stringify(data)
                }
              />
            );
          })}
        </Table>
      )}
    </div>
  );
};
