import React from 'react';
import {
  compareRFArguments,
  compareRemoteFields,
  findRemoteField,
  findRemoteFieldArgument,
  findArgParentField,
  defaultArgValue,
  RemoteRelationship,
  RemoteRelationshipServer,
  parseRemoteRelationship,
  TreeFieldElement,
  TreeArgElement,
  ArgValueKind,
} from './utils';
import { Table } from '../../../../../dataSources/types';

const getDefaultState = (table: Table): RemoteRelationship => ({
  name: '',
  remoteSchema: '',
  remoteFields: [],
  table: {
    name: table.table_name,
    schema: table.table_schema,
  },
});

export const setName = (data: string) => ({
  type: 'SetName' as const,
  data,
});

export const setRemoteSchema = (data: string) => ({
  type: 'SetRemoteSchema' as const,
  data,
});

export const toggleField = (data: TreeFieldElement) => ({
  type: 'ToggleField' as const,
  data,
});
export const toggleArg = (data: TreeArgElement) => ({
  type: 'ToggleArg' as const,
  data,
});

export const setArgValueKind = (
  arg: TreeArgElement,
  valueKind: ArgValueKind
) => ({
  type: 'ChangeArgValueKind' as const,
  data: {
    arg,
    valueKind,
  },
});
export const setArgValue = (arg: TreeArgElement, value: string) => ({
  type: 'ChangeArgValue' as const,
  data: {
    arg,
    value,
  },
});

export const resetState = (data: RemoteRelationship) => ({
  type: 'ResetState' as const,
  data,
});

export type Action =
  | ReturnType<typeof toggleField>
  | ReturnType<typeof toggleArg>
  | ReturnType<typeof setName>
  | ReturnType<typeof setRemoteSchema>
  | ReturnType<typeof setArgValueKind>
  | ReturnType<typeof setArgValue>
  | ReturnType<typeof resetState>;

const reducer = (
  state: RemoteRelationship,
  action: Action
): RemoteRelationship => {
  switch (action.type) {
    case 'SetName': {
      return {
        ...state,
        name: action.data,
      };
      break;
    }
    case 'SetRemoteSchema': {
      return {
        ...state,
        remoteSchema: action.data,
      };
      break;
    }
    case 'ToggleField': {
      const changedField = action.data;
      const selectedField = findRemoteField(state.remoteFields, changedField);
      if (selectedField) {
        return {
          ...state,
          remoteFields: state.remoteFields.filter(
            f => !(f.depth >= changedField.depth)
          ),
        };
      }
      return {
        ...state,
        remoteFields: [
          ...state.remoteFields.filter(f => !(f.depth >= changedField.depth)),
          { ...changedField, arguments: [] },
        ],
      };

      break;
    }
    case 'ToggleArg': {
      const changedArg = action.data;
      return {
        ...state,
        remoteFields: state.remoteFields.map(rf => {
          if (
            rf.name === changedArg.parentField &&
            rf.depth === changedArg.parentFieldDepth
          ) {
            const selectedArg = findRemoteFieldArgument(
              rf.arguments,
              changedArg
            );
            if (selectedArg) {
              return {
                ...rf,
                arguments: rf.arguments.filter(
                  a => !compareRFArguments(a, changedArg)
                ),
              };
            }
            return {
              ...rf,
              arguments: [
                ...rf.arguments,
                { ...changedArg, value: defaultArgValue },
              ],
            };
          }
          return rf;
        }),
      };
      break;
    }
    case 'ChangeArgValueKind': {
      const changedArg = action.data.arg;
      const parentField = findArgParentField(state.remoteFields, changedArg);
      if (parentField) {
        const newParentField = {
          ...parentField,
          arguments: parentField.arguments.map(a => {
            if (compareRFArguments(a, changedArg)) {
              return {
                ...a,
                type: action.data.valueKind === 'column' ? 'String' : a.type,
                value: {
                  ...a.value,
                  kind: action.data.valueKind,
                  value: '',
                },
              };
            }
            return a;
          }),
        };
        return {
          ...state,
          remoteFields: state.remoteFields.map(f => {
            if (compareRemoteFields(f, parentField)) {
              return newParentField;
            }
            return f;
          }),
        };
      }
      return state;
    }
    case 'ChangeArgValue': {
      const changedArg = action.data.arg;
      const parentField = findArgParentField(state.remoteFields, changedArg);
      if (parentField) {
        const newParentField = {
          ...parentField,
          arguments: parentField.arguments.map(a => {
            if (compareRFArguments(a, changedArg)) {
              return {
                ...a,
                value: {
                  ...a.value,
                  value: action.data.value,
                },
              };
            }
            return a;
          }),
        };
        return {
          ...state,
          remoteFields: state.remoteFields.map(f => {
            if (compareRemoteFields(f, parentField)) {
              return newParentField;
            }
            return f;
          }),
        };
      }
      return state;
    }
    case 'ResetState': {
      return action.data;
    }
    default:
      return state;
      break;
  }
};

// type "table" once ST PR is merged
export const useRemoteRelationship = (
  table: Table,
  relationship?: RemoteRelationshipServer
) => {
  const [state, dispatch] = React.useReducer(
    reducer,
    relationship
      ? parseRemoteRelationship(relationship)
      : getDefaultState(table)
  );

  const reset = () => {
    dispatch(
      resetState(
        relationship
          ? parseRemoteRelationship(relationship)
          : getDefaultState(table)
      )
    );
  };

  React.useEffect(reset, [relationship]);

  return {
    state,
    dispatch,
    reset,
  };
};
