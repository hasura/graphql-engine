import { useReducer } from 'react';
import produce from 'immer';

import { Selection } from '../../PermissionsTable';

export type MachineState =
  | 'closed'
  | 'formOpen'
  | 'bulkOpen'
  | 'updateRoleName';

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

export type MachineSend = (event: MachineEvents | 'CLOSE') => void;

interface MachineInternalState {
  value: MachineState;
  context: MachineCtx;
}

const initialState: MachineInternalState = {
  value: 'closed',
  context: {
    newRoleName: '',
    selectedForm: {},
    bulkSelections: [],
    isNewRole: false,
  },
};

const reducer = (
  state: MachineInternalState,
  event: MachineEvents
): MachineInternalState => {
  const { context } = state;

  switch (event.type) {
    case 'FORM_OPEN': {
      const newContext: MachineCtx = {
        ...context,
        selectedForm: event.selectedForm,
        bulkSelections: [],
      };

      if (!event.selectedForm.isNewRole) {
        newContext.newRoleName = '';
      }

      const nextValue: MachineState =
        newContext.newRoleName === '' && event.selectedForm.isNewRole === true
          ? 'updateRoleName'
          : 'formOpen';

      return { value: nextValue, context: newContext };
    }
    case 'BULK_OPEN': {
      const bulkSelections = produce(context.bulkSelections, draft => {
        if (draft.includes(event.roleName)) {
          const idx = draft.indexOf(event.roleName);
          draft.splice(idx, 1);
        } else {
          draft.push(event.roleName);
        }
      });

      return {
        value: bulkSelections.length === 0 ? 'closed' : 'bulkOpen',
        context: {
          newRoleName: '',
          selectedForm: {},
          bulkSelections,
        },
      };
    }
    case 'NEW_ROLE_NAME': {
      return {
        value: 'updateRoleName',
        context: {
          selectedForm: {},
          bulkSelections: [],
          newRoleName: event.newRoleName,
        },
      };
    }
    case 'CLOSE': {
      if (state.value === 'closed') return state;
      return {
        value: 'closed',
        context: {
          newRoleName: '',
          selectedForm: {},
          bulkSelections: [],
        },
      };
    }
    default:
      return state;
  }
};

export const useTableMachine = (): [MachineInternalState, MachineSend] => {
  const [state, dispatch] = useReducer(reducer, initialState);

  const send: MachineSend = event => {
    dispatch(typeof event === 'string' ? { type: event } : event);
  };

  return [state, send];
};

export type TableMachine = typeof useTableMachine;
