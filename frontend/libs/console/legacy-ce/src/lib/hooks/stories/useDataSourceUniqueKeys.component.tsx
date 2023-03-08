import { Table, TableRow, TableHeader } from '../../components/Common/Table';
import { currentDriver, Driver, setDriver } from '../../dataSources';

import { useAppDispatch, useAppSelector } from '../../storeHooks';
import React from 'react';
import { useDataSourceUniqueKeys, useSchemaList } from '..';

export const AllUniqueKeys = ({
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
    data: uniqueKeys,
    isLoading,
    isSuccess,
    isError: isError2,
    isIdle,
    error: fkError,
  } = useDataSourceUniqueKeys(
    { schemas: schemas!, source, driver: currentDriver },
    {
      enabled: !!schemas,
      retry: 0,
    }
  );
  const keys = React.useMemo(
    () => Object.keys(uniqueKeys?.[0] ?? {}),
    [uniqueKeys]
  );

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
      <h1 className="prose-xl">useDataSourceUniqueKeys</h1>
      <p className="prose-lg mb-8">
        Unique Constraints on <b>{schemas?.join(', ')}</b> schemas
      </p>
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
