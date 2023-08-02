// This file was automatically generated. Edits will be overwritten

export interface Typegen0 {
  '@@xstate/typegen': true;
  internalEvents: {
    '': { type: '' };
    'xstate.init': { type: 'xstate.init' };
  };
  invokeSrcNameMap: {};
  missingImplementations: {
    actions: never;
    delays: never;
    guards: never;
    services: never;
  };
  eventsCausingActions: {
    bulkUpdateEffect: 'BULK_OPEN';
    formCloseEffect: '' | 'CLOSE' | 'xstate.init';
    formOpenEffect: 'FORM_OPEN';
    updateRoleNameEffect: '' | 'NEW_ROLE_NAME';
  };
  eventsCausingDelays: {};
  eventsCausingGuards: {
    bulkIsEmpty: '';
    newRoleEmpty: '';
  };
  eventsCausingServices: {};
  matchesStates: 'bulkOpen' | 'closed' | 'formOpen' | 'updateRoleName';
  tags: never;
}
