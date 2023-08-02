import { currentDriver, Driver, setDriver } from '../../dataSources';

import { useAppDispatch, useAppSelector } from '../../storeHooks';
import React from 'react';
import { useSchemaList } from '..';

export const SchemaList = ({
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
    isLoading,
    isSuccess,
  } = useSchemaList(
    {
      source,
      driver: currentDriver,
    },
    { retry: 0 }
  );

  if (isError) {
    return <p className="text-red-500">Error: {error?.message}</p>;
  }

  if (isLoading) {
    return <p>Loading...</p>;
  }

  return (
    <div>
      <h1 className="prose-xl">useTrackableFunctions</h1>
      <p className="prose-lg mb-8">
        Schemas on <b>{source}</b> data source
      </p>
      {isSuccess && (
        <pre>
          <code>{JSON.stringify(schemas, null, 4)}</code>
        </pre>
      )}
    </div>
  );
};
