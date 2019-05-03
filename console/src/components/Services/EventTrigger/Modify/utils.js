import semverCheck from '../../../../helpers/semver';

export const CONSOLE_QUERY = 'Allow invoking this trigger via data browser';
export const INTERNAL_CONSOLE_QUERY_REP = 'enable_manual';
export const getValidQueryTypes = version => {
  const defaultQueryTypes = ['insert', 'update', 'delete'];
  if (semverCheck('manualTriggers', version)) {
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

export const getManualOperationValue = (queryType, definition) => {
  if (queryType in definition) {
    return definition[queryType];
  }
  return true;
};
