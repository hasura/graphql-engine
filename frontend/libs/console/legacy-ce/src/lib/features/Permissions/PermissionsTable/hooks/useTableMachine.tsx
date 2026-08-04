import { createMachine, assign } from 'xstate';
import { useMachine } from '@xstate/react';
import produce from 'immer';

import { Selection } from '../../PermissionsTable';

export interface MachineCtx {
  newRoleName: string;
  selectedForm: Partial<Selection>;
  bulkSelections: string[];
  isNewRole?: boolean;
}

export type MachineEvents =
  | { type: 'FORM_OPEN'; selectedForm: Partial<Selection> }
  | { type: 'BULK_OPEN'; roleName: string }
  | { type: 'NEW_ROLE_NAME'; newRoleName: string }
  | { type: 'CLOSE' };

export const tableMachine = createMachine(
  {
    tsTypes: {} as import('./useTableMachine.typegen').Typegen0,
    schema: {
      context: {} as MachineCtx,
      events: {} as MachineEvents,
    },
    id: 'tableMachine',
    initial: 'closed',
    context: {
      newRoleName: '',
      selectedForm: {},
      bulkSelections: [],
      isNewRole: false,
    },
    states: {
      closed: {
        onEntry: ['formCloseEffect'],
        on: {
          FORM_OPEN: 'formOpen',
          BULK_OPEN: 'bulkOpen',
          NEW_ROLE_NAME: 'updateRoleName',
        },
      },
      formOpen: {
        onEntry: ['formOpenEffect'],
        always: {
          target: 'updateRoleName',
          cond: 'newRoleEmpty',
        },
        on: {
          CLOSE: 'closed',
          FORM_OPEN: 'formOpen',
          BULK_OPEN: 'bulkOpen',
          NEW_ROLE_NAME: 'updateRoleName',
        },
      },
      bulkOpen: {
        onEntry: ['bulkUpdateEffect'],
        always: {
          target: 'closed',
          cond: 'bulkIsEmpty',
        },
        on: {
          CLOSE: 'closed',
          FORM_OPEN: 'formOpen',
          BULK_OPEN: 'bulkOpen',
          NEW_ROLE_NAME: 'updateRoleName',
        },
      },
      updateRoleName: {
        onEntry: ['updateRoleNameEffect'],
        on: {
          NEW_ROLE_NAME: 'updateRoleName',
          FORM_OPEN: 'formOpen',
          BULK_OPEN: 'bulkOpen',
        },
      },
    },
  },
  {
    actions: {
      formCloseEffect: assign(() => {
        return {
          newRoleName: '',
          selectedForm: {},
          bulkSelections: [] as string[],
        };
      }),
      formOpenEffect: assign((ctx: any, evt: any) => {
        const obj = {
          ...ctx,
          selectedForm: evt.selectedForm,
          bulkSelections: [] as string[],
        };

        if (!evt.selectedForm.isNewRole) {
          obj.newRoleName = '';
        }

        return obj;
      }),
      bulkUpdateEffect: assign((ctx: any, evt: any) => {
        const bulkSelections = produce(ctx.bulkSelections, (draft: any) => {
          if (draft.includes(evt.roleName)) {
            const idx = draft.indexOf(evt.roleName);
            draft.splice(idx, 1);
          } else {
            draft.push(evt.roleName);
          }
        });

        return {
          newRoleName: '',
          selectedForm: {},
          bulkSelections,
        };
      }),
      updateRoleNameEffect: assign((_ctx: any, evt: any) => {
        if (evt.type !== 'NEW_ROLE_NAME') {
          return {
            selectedForm: {},
            bulkSelections: [],
            newRoleName: '',
          };
        }

        return {
          selectedForm: {},
          bulkSelections: [],
          newRoleName: evt.newRoleName,
        };
      }),
    },
    guards: {
      bulkIsEmpty: (ctx: any) => ctx.bulkSelections.length === 0,
      newRoleEmpty: (ctx: any) =>
        ctx.newRoleName === '' && ctx.selectedForm.isNewRole === true,
    },
  }
);

export const useTableMachine = () => useMachine(tableMachine);

export type TableMachine = typeof useTableMachine;
