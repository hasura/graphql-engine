import type { LogicalModel } from './logicalModel';
import { Table } from './table';

export type StoredProcedureArgument = {
  type: string;
  nullable?: boolean;
};

export type QualifiedStoredProcedure = Table;

export type StoredProcedure = {
  stored_procedure: QualifiedStoredProcedure;
  returns: LogicalModel['name'];
  configuration: {
    custom_name?: string;
    // this will exposed as query and mutation later on
    exposed_as: 'query' | 'mutation';
  };
  arguments?: Record<string, StoredProcedureArgument>;
};
