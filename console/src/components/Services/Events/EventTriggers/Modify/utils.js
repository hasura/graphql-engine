export const MANUAL_TRIGGER_TEXT = 'Via console';
export const MANUAL_TRIGGER_VAR = 'enable_manual';

export const getTriggerOperations = () => {
  return ['insert', 'update', 'delete', MANUAL_TRIGGER_TEXT];
};

export const triggerOperationMap = {
  insert: 'insert',
  update: 'update',
  delete: 'delete',
  [MANUAL_TRIGGER_TEXT]: MANUAL_TRIGGER_VAR,
};
