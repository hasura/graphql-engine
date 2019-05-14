export const MANUAL_TRIGGER_TEXT = 'Via console';
export const MANUAL_TRIGGER_VAR = 'enable_manual';

export const getValidQueryTypes = () => {
  const defaultQueryTypes = ['insert', 'update', 'delete'];

  defaultQueryTypes.push(MANUAL_TRIGGER_TEXT);

  return defaultQueryTypes;
};

export const queryToInternalNameMap = {
  insert: 'insert',
  update: 'update',
  delete: 'delete',
  [MANUAL_TRIGGER_TEXT]: MANUAL_TRIGGER_VAR,
};
