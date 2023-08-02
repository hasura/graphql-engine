import { RunSQLResponse } from '../../../../Datasources/types';

export const mssqlStoredProceduresMockResponse: RunSQLResponse = {
  result_type: 'TuplesOk',
  result: [
    ['routine_name', 'routine_schema'],
    ['stored_procedure_1', 'dbo'],
    ['stored_procedure_2', 'dbo'],
    ['stored_procedure_3', 'dbo'],
    ['stored_procedure_4', 'dbo'],
  ],
};
