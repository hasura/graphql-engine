/* eslint-disable no-use-before-define */

import { camelize } from 'inflection';
import { filterNameLessTypeLess } from '../../../../shared/utils/hasuraCustomTypeUtils';
import {
  getSchemaTypeMetadata,
  wrapTypename,
  unwrapType,
} from '../../../../shared/utils/wrappingTypeUtils';
import {
  isInputObjectType,
  isEnumType,
  isScalarType,
  isObjectType,
} from 'graphql';

import { gqlInbuiltTypes } from './stateDefaults';
import { getActionArguments, getActionOutputType, findType } from '../utils';
import { getConfirmation } from '../../../Common/utils/jsUtils';

export const isInbuiltType = typename => {
  return !!gqlInbuiltTypes.find(t => t.name === typename);
};

export const generateActionDefinition = ({
  arguments: args,
  outputType,
  kind = 'synchronous',
  webhook,
}) => {
  return {
    arguments: filterNameLessTypeLess(args),
    kind,
    output_type: outputType,
    webhook,
  };
};

export const getStateValidationError = ({ webhook }) => {
  if (!webhook) return 'Webhook cannot be empty';
  try {
    new URL(webhook); // eslint-disable-line
  } catch (e) {
    return 'Webhook must be a valid URL';
  }
  return null;
};

export const deriveExistingType = (
  selectedExistingType,
  existingTypemap,
  namespace
) => {
  const namespaceTypename = name => {
    if (namespace === '_') {
      return `_${name}`;
    }
    return camelize(`${namespace}_${name}`);
  };

  const types = {};

  const handleScalarType = (type, typename) => {
    types[type.name] = {
      name: typename,
      kind: 'scalar',
    };
  };

  const handleEnumType = (type, typename) => {
    types[typename] = {
      name: typename,
      kind: 'enum',
      values: type._values.map(v => ({
        value: v.value,
        description: v.description,
      })),
    };
  };

  const handleObjectType = (type, typename) => {
    types[typename] = {
      name: typename,
      kind: 'object',
      fields: [],
    };

    const parentTypes = [];

    Object.values(type._fields).forEach(f => {
      const _f = {
        name: f.name,
        description: f.description,
        arguments: [],
      };

      const fieldTypeMetadata = getSchemaTypeMetadata(f.type);
      if (!isScalarType(existingTypemap[fieldTypeMetadata.typename])) {
        return;
      }
      let namespacedTypename = fieldTypeMetadata.typename;
      if (!isInbuiltType(fieldTypeMetadata.typename)) {
        namespacedTypename = namespaceTypename(fieldTypeMetadata.typename);
        parentTypes.push(fieldTypeMetadata.typename);
      }

      _f.type = wrapTypename(namespacedTypename, fieldTypeMetadata.stack);
      _f.arguments = f.arguments
        ? f.arguments.map(a => {
          const _a = {
            name: a.name,
            description: a.description,
          };

          const argTypeMetadata = getSchemaTypeMetadata(a.type);
          let namespacedArgTypename = argTypeMetadata.typename;
          if (!isInbuiltType(argTypeMetadata.typename)) {
            namespacedArgTypename = namespaceTypename(
              argTypeMetadata.typename
            );
            parentTypes.push(argTypeMetadata.typename);
          }
          _a.type = wrapTypename(
            namespacedArgTypename,
            argTypeMetadata.stack
          );
          return _a;
        })
        : [];

      types[typename].fields.push(_f);
    });

    parentTypes.forEach(t => {
      handleType(existingTypemap[t], namespaceTypename(t));
    });
  };

  const handleInputObjectType = (type, typename) => {
    const _type = {
      name: typename,
      kind: 'input_object',
      fields: [],
    };

    const parentTypes = [];

    Object.values(type._fields).forEach(f => {
      const _f = {
        name: f.name,
        description: f.description,
      };

      const fieldTypeMetadata = getSchemaTypeMetadata(f.type);
      let namespacedTypename = fieldTypeMetadata.typename;
      if (!isInbuiltType(fieldTypeMetadata.typename)) {
        namespacedTypename = namespaceTypename(fieldTypeMetadata.typename);
        parentTypes.push(fieldTypeMetadata.typename);
      }

      _f.type = wrapTypename(namespacedTypename, fieldTypeMetadata.stack);
      _type.fields.push(_f);
    });

    types[typename] = _type;

    parentTypes.forEach(t => {
      handleType(existingTypemap[t], namespaceTypename(t));
    });
  };

  const handleType = (type, typename) => {
    if (isInbuiltType(type.name)) return;

    if (types[typename]) return;

    if (isScalarType(type)) {
      handleScalarType(type, typename);
      return;
    }

    if (isEnumType(type)) {
      handleEnumType(type, typename);
      return;
    }

    if (isInputObjectType(type)) {
      handleInputObjectType(type, typename);
      return;
    }

    if (isObjectType(type)) {
      handleObjectType(type, typename);
    }
  };

  handleType(
    existingTypemap[selectedExistingType],
    namespaceTypename(selectedExistingType)
  );

  return Object.values(types);
};

export const getActionTypes = (currentAction, allTypes) => {
  const actionTypes = {};
  const actionArgs = getActionArguments(currentAction);
  const actionOutputType = getActionOutputType(currentAction);

  const getDependentTypes = maybeWrappedTypename => {
    const { typename } = unwrapType(maybeWrappedTypename);
    if (isInbuiltType(typename)) return;
    if (actionTypes[typename]) return;

    const type = findType(allTypes, typename);
    actionTypes[typename] = type;

    if (type.fields) {
      type.fields.forEach(f => {
        getDependentTypes(f.type);
        if (f.arguments) {
          f.arguments.forEach(a => {
            getDependentTypes(a.type);
          });
        }
      });
    }
  };

  actionArgs.forEach(a => {
    getDependentTypes(a.type);
  });

  getDependentTypes(actionOutputType);

  return Object.values(actionTypes);
};

export const getOverlappingTypeConfirmation = (
  currentActionName,
  allActions,
  allTypes,
  overlappingTypenames
) => {
  const otherActions = allActions.filter(
    a => a.action_name !== currentActionName
  );

  const typeCollisionMap = {};

  for (let i = otherActions.length - 1; i >= 0; i--) {
    const action = otherActions[i];
    const actionTypes = getActionTypes(action, allTypes);
    actionTypes.forEach(t => {
      if (typeCollisionMap[t.name]) return;
      overlappingTypenames.forEach(ot => {
        if (ot === t.name) {
          typeCollisionMap[ot] = true;
        }
      });
    });
  }

  let isOk = true;
  const collidingTypes = Object.keys(typeCollisionMap);
  const numCollidingTypes = collidingTypes.length;

  if (numCollidingTypes) {
    const types = `${collidingTypes.join(', ')}`;
    const typeLabel = numCollidingTypes === 1 ? 'type' : 'types';
    const verb = numCollidingTypes === 1 ? 'is' : 'are';
    isOk = getConfirmation(
      `The ${typeLabel} "${types}" ${verb} also used by other actions. Your current type definition will replace the existing type definition. This will impact existing actions`
    );
  }

  return isOk;
};
