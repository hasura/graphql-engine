// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  '@@xstate/typegen': true;
  eventsCausingActions: {
    formCloseEffect: 'CLOSE' | '';
    formOpenEffect: 'FORM_OPEN';
    bulkUpdateEffect: 'BULK_OPEN';
    updateRoleNameEffect: 'NEW_ROLE_NAME' | '';
  };
  internalEvents: {
    '': { type: '' };
    'xstate.init': { type: 'xstate.init' };
  };
  invokeSrcNameMap: {};
  missingImplementations: {
    actions: never;
    services: never;
    guards: never;
    delays: never;
  };
  eventsCausingServices: {};
  eventsCausingGuards: {
    newRoleEmpty: '';
    bulkIsEmpty: '';
  };
  eventsCausingDelays: {};
  matchesStates: 'closed' | 'formOpen' | 'bulkOpen' | 'updateRoleName';
  tags: never;
}
