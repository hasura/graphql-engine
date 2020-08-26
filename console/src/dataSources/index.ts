/* eslint-disable import/no-mutable-exports */
import { useState, useEffect } from 'react';
import { services } from './services';

import { Table, ComputedField, TableColumn } from './types';
import { PGFunction } from './services/postgresql/types';
import { Operations } from './common';

export type Driver = 'postgres'; // | 'mysql';

export type ColumnsInfoResult = {
  [tableName: string]: {
    [columnName: string]: {
      is_generated: boolean;
      is_identity: boolean;
      identity_generation: 'ALWAYS' | 'BY DEFAULT' | string | null;
    };
  };
};

export interface DataSourcesAPI {
  isTable(table: Table): boolean;
  displayTableName(table: Table): JSX.Element;
  // todo: replace with function type
  getFunctionSchema(func: PGFunction): string;
  getFunctionName(func: PGFunction): string;
  getFunctionDefinition(func: PGFunction): string;
  getSchemaFunctions(func: PGFunction[], schemaName: string): PGFunction[];
  findFunction(
    func: PGFunction[],
    fName: string,
    fSchema: string
  ): PGFunction | undefined;
  getGroupedTableComputedFields(
    table: Table,
    allFunctions: PGFunction[]
  ): {
    scalar: ComputedField[];
    table: ComputedField[];
  };
  isColumnAutoIncrement(col: TableColumn): boolean;
  getTableSupportedQueries(table: Table): Operations[];
  getColumnType(col: TableColumn): string;
  arrayToPostgresArray(arr: any[]): string;
  initQueries: {
    schemaList: Record<string, any>;
    loadTrackedFunctions: Record<string, any>;
    loadTrackableFunctions: Record<string, any>;
    loadNonTrackableFunctions: Record<string, any>;
  };
  additionalColumnsInfoQuery: (schemaName: string) => Record<string, any>;
  parseColumnsInfoResult: (data: any) => ColumnsInfoResult;
  columnDataTypes: {
    INTEGER: string;
    SERIAL: string;
    BIGINT: string;
    BIGSERIAL: string;
    UUID: string;
    JSONDTYPE: string;
    JSONB: string;
    TIMESTAMP: string;
    TIME: string;
    NUMERIC: string;
    DATE: string;
    TIMETZ: string;
    BOOLEAN: string;
    TEXT: string;
    ARRAY: string;
  };
  generateWhereClause: (
    options: {
      schemas: string[];
      tables: Table[];
    },
    sqlTableName?: string,
    sqlSchemaName?: string,
    clausePrefix?: string
  ) => string;
  getFetchTrackedTableFkQuery: (options: {
    schemas: string[];
    tables: Table[];
  }) => string;
  getFetchTrackedTableReferencedFkQuery: (options: {
    schemas: string[];
    tables: Table[];
  }) => string;
  getFetchTablesListQuery: (options: {
    schemas: string[];
    tables: Table[];
  }) => string;
}

export let currentDriver: Driver = 'postgres';
export let dataSource: DataSourcesAPI = services[currentDriver];

class DataSourceChangedEvent extends Event {
  static type = 'data-source-changed';
  constructor(public driver: Driver) {
    super(DataSourceChangedEvent.type);
  }
}
const eventTarget = new EventTarget();

export const setDriver = (driver: Driver) => {
  currentDriver = driver;
  dataSource = services[driver];

  eventTarget.dispatchEvent(new DataSourceChangedEvent(driver));
};

export const useDataSource = (): {
  driver: Driver;
  setDriver: (driver: Driver) => void;
  dataSource: DataSourcesAPI;
} => {
  const [driver, setState] = useState(currentDriver);

  useEffect(() => {
    const handleDriverChange = (event: Event) => {
      if (event instanceof DataSourceChangedEvent) {
        setState(event.driver);
      }
    };

    eventTarget.addEventListener(
      DataSourceChangedEvent.type,
      handleDriverChange
    );

    return () => {
      eventTarget.removeEventListener(
        DataSourceChangedEvent.type,
        handleDriverChange
      );
    };
  });

  return {
    driver,
    dataSource,
    setDriver,
  };
};

if ((module as any).hot) {
  // // todo
  // (module as any).hot.dispose((data: any) => {
  //   data.driver = currentDriver;
  // });
  // (module as any).hot.accept(['./postgres'], () => {
  //   currentDriver = (module as any).hot.data.driver;
  // });
}

export * from './common';
