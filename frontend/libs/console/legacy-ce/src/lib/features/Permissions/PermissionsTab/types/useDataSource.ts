import { currentDriver } from '../../../../dataSources';
import { useAppSelector } from '../../../../storeHooks';
import { NewDataSource } from './types';

export const useDataSource = () => {
  const database: string = useAppSelector(
    state => state.tables.currentDataSource
  );
  const driver = currentDriver;
  return {
    driver,
    database,
  } as NewDataSource;
};
