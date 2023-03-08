import { Driver } from '../dataSources';

export type RunSQLResponse =
  | {
      result: string[][];
      result_type: 'TuplesOk';
    }
  | {
      result_type: 'CommandOk';
      result: null;
    };

export type QualifiedDataSource = {
  source: string;
  driver: Driver;
};
