export type RunSQLSelectResponse = {
  result_type: 'TuplesOk';
  result: string[][];
};

export type RunSQLCommandResponse = {
  result_type: 'CommandOk';
  result: null;
};

export type RunSQLResponse = RunSQLSelectResponse | RunSQLCommandResponse;
