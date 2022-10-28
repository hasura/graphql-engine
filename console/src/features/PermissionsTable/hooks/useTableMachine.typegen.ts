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
    services: never;
    guards: never;
    delays: never;
  };
  eventsCausingActions: {
    bulkUpdateEffect: 'BULK_OPEN';
    formCloseEffect: '' | 'CLOSE' | 'xstate.init';
    formOpenEffect: 'FORM_OPEN';
    updateRoleNameEffect: '' | 'NEW_ROLE_NAME';
  };
  eventsCausingServices: {};
  eventsCausingGuards: {
    bulkIsEmpty: '';
    newRoleEmpty: '';
  };
  eventsCausingDelays: {};
  matchesStates: 'bulkOpen' | 'closed' | 'formOpen' | 'updateRoleName';
  tags: never;
}
