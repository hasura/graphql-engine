import { Table, TableRow, TableHeader } from '../../components/Common/Table';
import { currentDriver, Driver, setDriver } from '../../dataSources';
import { QualifiedTable } from '../../metadata/types';
import { useAppDispatch, useAppSelector } from '../../storeHooks';
import React from 'react';
import { useTableCheckConstraints } from '..';

export const CheckConstaints = ({
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
    data: checkConstraints,
    isLoading,
    isSuccess,
    isError,
    error,
  } = useTableCheckConstraints({ table, source, driver: currentDriver });
  const keys = React.useMemo(
    () => Object.keys(checkConstraints?.[0] ?? {}),
    [checkConstraints]
  );

  if (isError) {
    return <p className="text-red-500">Error: {error?.message}</p>;
  }

  if (isLoading) {
    return <p>Loading...</p>;
  }

  return (
    <div>
      <h1 className="prose-xl">useTableCheckConstraints</h1>
      <p className="prose-lg mb-8">
        Check Constraints on{' '}
        <b>
          {table.schema}.{table.name}
        </b>{' '}
        table
      </p>
      {isSuccess &&
        !checkConstraints?.length &&
        "Table either doesn't exist or doesn't have Check constriants"}
      {isSuccess && (
        <Table
          rowCount={checkConstraints?.length ?? 0}
          columnCount={keys?.length ?? 0}
        >
          <TableHeader headers={keys} />
          {checkConstraints?.map((fk, i) => {
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
