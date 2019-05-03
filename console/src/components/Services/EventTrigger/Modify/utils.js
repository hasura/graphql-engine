import semverCheck from '../../../../helpers/semver';

const CONSOLE_QUERY = 'Allow invoking this trigger via Data browser';
export const INTERNAL_CONSOLE_QUERY_REP = 'enable_manual';
export const getValidQueryTypes = () => {
  const defaultQueryTypes = ['insert', 'update', 'delete'];
  if (semverCheck('manualTriggers')) {
    defaultQueryTypes.push(CONSOLE_QUERY);
  }
  return defaultQueryTypes;
};

export const queryToInternalNameMap = {
  [CONSOLE_QUERY]: INTERNAL_CONSOLE_QUERY_REP,
  insert: 'insert',
  update: 'update',
  delete: 'delete',
};
