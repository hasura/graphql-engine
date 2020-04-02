declare function makeMigrationCall(
  dispatch: any,
  getState: any,
  upQueries: Array<any>,
  downQueries: Array<any>,
  migrationName: string,
  customOnSuccess: () => void,
  customOnError: () => void,
  requestMsg: string,
  successMsg: string,
  errorMsg: string,
  shouldSkipSchemaReload: boolean
): void;

export { makeMigrationCall };
