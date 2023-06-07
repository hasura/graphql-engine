import { Table, TableRow, TableHeader } from '../../components/Common/Table';
import { currentDriver, Driver, setDriver } from '../../dataSources';
import { QualifiedFunction } from '../../metadata/types';

import { useAppDispatch, useAppSelector } from '../../storeHooks';
import React from 'react';
import { useSchemaList, useSingleFunction } from '..';

export const SingleFunction = ({
  driver,
  currentDatasource,
  myFunction,
}: {
  driver: Driver;
  currentDatasource: string;
  myFunction: QualifiedFunction;
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
    data: singleFn,
    isLoading,
    isSuccess,
    isError: isError2,
    isIdle,
    error: fkError,
  } = useSingleFunction(
    {
      fn: myFunction,
      source,
      driver: currentDriver,
    },
    {
      enabled: !!schemas,
      retry: 0,
    }
  );
  const keys = React.useMemo(() => Object.keys(singleFn ?? {}), [singleFn]);
  const entries = React.useMemo(
    () => Object.values(singleFn ?? {}),
    [singleFn]
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
      <h1 className="prose-xl">useSingleFunction</h1>
      <p className="prose-lg mb-8">
        <b>
          {myFunction.schema}.{myFunction.name}
        </b>{' '}
        function
      </p>
      {isSuccess && (
        <Table rowCount={1} columnCount={keys?.length ?? 0}>
          <TableHeader headers={keys.length ? keys : ['EmptyValue']} />
          <TableRow
            entries={entries.length ? entries : ['null']}
            index=""
            readonly
            renderCol={({ data }) =>
              typeof data === 'string' ? data : JSON.stringify(data)
            }
          />
        </Table>
      )}
    </div>
  );
};
