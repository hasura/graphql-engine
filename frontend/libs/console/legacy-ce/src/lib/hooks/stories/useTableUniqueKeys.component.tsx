import { Table, TableRow, TableHeader } from '../../components/Common/Table';
import { currentDriver, Driver, setDriver } from '../../dataSources';
import { QualifiedTable } from '../../metadata/types';
import { useAppDispatch, useAppSelector } from '../../storeHooks';
import React from 'react';
import { useTableUniqueKeys } from '..';

export const UniqueKeys = ({
  driver,
  currentDatasource,
  table,
}: {
  driver: Driver;
  currentDatasource: string;
  table: QualifiedTable;
}) => {
  const dispatch = useAppDispatch();
  const source = useAppSelector(s => s.tables.currentDataSource);

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

  const {
    data: uniqueKeys,
    isLoading,
    isSuccess,
    isError,
    error,
  } = useTableUniqueKeys({ table, source, driver: currentDriver });
  const keys = React.useMemo(
    () => Object.keys(uniqueKeys?.[0] ?? {}),
    [uniqueKeys]
  );

  if (isError) {
    return <p className="text-red-500">Error: {error?.message}</p>;
  }

  if (isLoading) {
    return <p>Loading...</p>;
  }

  return (
    <div>
      <h1 className="prose-xl">useTableUniqueKeys</h1>
      <p className="prose-lg mb-8">
        Unique Constraints on{' '}
        <b>
          {table.schema}.{table.name}
        </b>{' '}
        table
      </p>
      {isSuccess &&
        !uniqueKeys?.length &&
        "Table either doesn't exist or doesn't have Unique constriants"}
      {isSuccess && (
        <Table
          rowCount={uniqueKeys?.length ?? 0}
          columnCount={keys?.length ?? 0}
        >
          <TableHeader headers={keys} />
          {uniqueKeys?.map((fk, i) => {
            return (
              <TableRow
                entries={Object.values(fk)}
                index=""
                key={i}
                readonly
                renderCol={({ data }) =>
                  typeof data === 'string' ? data : JSON.stringify(data)
                }
              />
            );
          })}
        </Table>
      )}
    </div>
  );
};
