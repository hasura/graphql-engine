export type AlterFKTableInfo = {
  tableName: string;
  schemaName: string;
  columns: string[];
};

export type MySQLTrigger = {
  action_timing: 'BEFORE' | 'AFTER' | string;
  event_manipulation: 'INSERT' | 'DELETE' | 'UPDATE' | string;
  action_orientation: 'ROW' | string;
  action_statement: string;
};

export type CreatePKArgs = {
  schemaName: string;
  tableName: string;
  selectedPkColumns: string[];
};
