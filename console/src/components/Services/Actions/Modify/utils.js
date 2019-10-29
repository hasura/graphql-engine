import { getActionTypes } from '../../Types/utils';
import { unwrapType } from '../../Types/wrappingTypeUtils';
import {
  defaultArg,
  defaultField,
  defaultEnumValue,
  defaultScalarType,
  gqlInbuiltTypes,
} from '../Common/stateDefaults';

export const getModifyState = (currentAction, allTypes) => {
  const { action_defn: actionDef } = currentAction;
  const actionTypes = [
    ...gqlInbuiltTypes,
    ...getActionTypes(actionDef, allTypes),
  ];
  const modifyState = {};
  modifyState.name = currentAction.action_name;
  modifyState.webhook = actionDef.webhook;
  modifyState.kind = actionDef.kind;
  modifyState.arguments = actionDef.arguments.map(a => {
    const _a = { ...a };
    const { typename: argType, index: typeWrapperIndex } = unwrapType(a.type);
    const argTypeIndex = actionTypes
      .findIndex(t => t.name === argType)
      .toString();
    _a.type = argTypeIndex;
    _a.typeWrap = typeWrapperIndex.toString();
    return _a;
  });
  modifyState.arguments.push(defaultArg);
  modifyState.outputType = actionTypes
    .findIndex(t => t.name === actionDef.output_type)
    .toString();
  modifyState.types = actionTypes.map(t => {
    const _t = { ...t, isModifying: true };
    if (t.isInbuilt) return _t;
    switch (t.kind) {
      case 'scalar':
        return _t;
      case 'object':
        _t.arguments = [];
        _t.arguments.push(defaultArg);
        _t.fields = _t.fields.map(f => {
          const { typename, index: typeWrap } = unwrapType(f.type);
          return {
            ...f,
            type: actionTypes
              .findIndex(__t => __t.name === typename)
              .toString(),
            typeWrap: typeWrap.toString(),
          };
        });
        _t.fields.push(defaultField);
        return _t;
      case 'input_object':
        _t.fields = _t.fields.map(f => {
          const { typename, index: typeWrap } = unwrapType(f.type);
          return {
            ...f,
            type: actionTypes
              .findIndex(__t => __t.name === typename)
              .toString(),
            typeWrap: typeWrap.toString(),
          };
        });
        _t.fields.push(defaultField);
        return _t;
      case 'enum':
        _t.values.push(defaultEnumValue);
        return _t;
      default:
        return _t;
    }
  });
  modifyState.types.push({ ...defaultScalarType });
  return modifyState;
};
