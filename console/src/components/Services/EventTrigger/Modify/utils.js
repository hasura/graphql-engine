import semverCheck from '../../../../helpers/semver';

const CONSOLE_QUERY = 'console';
export const INTERNAL_CONSOLE_QUERY_REP = 'manual';
export const getValidQueryTypes = () => {
  const defaultQueryTypes = ['insert', 'update', 'delete'];
  if (semverCheck('manualTriggers')) {
    defaultQueryTypes.push(CONSOLE_QUERY);
  }
  return defaultQueryTypes;
};

export const queryToInternalNameMap = {
  console: 'manual',
  insert: 'insert',
  update: 'update',
  delete: 'delete',
};
