/* eslint-disable no-use-before-define */

import { camelize } from 'inflection';
import { filterNameLessTypeLess } from '../../../../shared/utils/hasuraCustomTypeUtils';
import {
  getSchemaTypeMetadata,
  wrapTypename,
  unwrapType,
} from '../../../../shared/utils/wrappingTypeUtils';
import { inbuiltTypes } from '../../../../shared/utils/hasuraCustomTypeUtils';
import {
  isInputObjectType,
  isEnumType,
  isScalarType,
  isObjectType,
} from 'graphql';

import { getActionArguments, getActionOutputType, findType } from '../utils';
import {
  getConfirmation,
  isValidTemplateLiteral,
} from '../../../Common/utils/jsUtils';
import { transformHeaders } from '../../../Common/Headers/utils';

export const isInbuiltType = typename => {
  return !!inbuiltTypes[typename];
};

export const generateActionDefinition = ({
  arguments: args,
  outputType,
  kind = 'synchronous',
  handler,
  actionType,
  headers,
  forwardClientHeaders,
  timeout,
}) => {
  return {
    arguments: filterNameLessTypeLess(args),
    kind,
    output_type: outputType,
    handler,
    type: actionType,
    headers: transformHeaders(headers),
    forward_client_headers: forwardClientHeaders,
    timeout,
  };
};

export const getStateValidationError = ({ handler }) => {
  if (!handler) return 'Handler cannot be empty';
  if (isValidTemplateLiteral(handler)) return null;
  try {
    new URL(handler); // eslint-disable-line
  } catch (e) {
    return 'Handler must be a valid URL or a template';
  }
  return null;
};

export const deriveExistingType = (
  selectedExistingType,
  existingTypemap,
  prefix
) => {
  const prefixTypename = name => {
    if (prefix === '_') {
      return `_${name}`;
    }
    return camelize(`${prefix}_${name}`);
  };

  const types = {};

  const getEntityDescription = type => {
    return type.description
      ? type.description.replace('"', '"')
      : type.description;
  };

  const handleScalarType = (type, typename) => {
    types[type.name] = {
      name: typename,
      kind: 'scalar',
      description: getEntityDescription(type),
    };
  };

  const handleEnumType = (type, typename) => {
    types[typename] = {
      name: typename,
      kind: 'enum',
      values: type._values.map(v => ({
        value: v.value,
        description: getEntityDescription(v),
      })),
      description: getEntityDescription(type),
    };
  };

  const handleObjectType = (type, typename) => {
    types[typename] = {
      name: typename,
      kind: 'object',
      fields: [],
      description: getEntityDescription(type),
    };

    const parentTypes = [];

    Object.values(type._fields).forEach(f => {
      const _f = {
        name: f.name,
        description: getEntityDescription(f),
        arguments: [],
      };

      const fieldTypeMetadata = getSchemaTypeMetadata(f.type);
      if (!isScalarType(existingTypemap[fieldTypeMetadata.typename])) {
        return;
      }
      let prefixdTypename = fieldTypeMetadata.typename;
      if (!isInbuiltType(fieldTypeMetadata.typename)) {
        prefixdTypename = prefixTypename(fieldTypeMetadata.typename);
        parentTypes.push(fieldTypeMetadata.typename);
      }

      _f.type = wrapTypename(prefixdTypename, fieldTypeMetadata.stack);
      _f.arguments = f.arguments
        ? f.arguments.map(a => {
            const _a = {
              name: a.name,
              description: getEntityDescription(a),
            };

            const argTypeMetadata = getSchemaTypeMetadata(a.type);
            let prefixdArgTypename = argTypeMetadata.typename;
            if (!isInbuiltType(argTypeMetadata.typename)) {
              prefixdArgTypename = prefixTypename(argTypeMetadata.typename);
              parentTypes.push(argTypeMetadata.typename);
            }
            _a.type = wrapTypename(prefixdArgTypename, argTypeMetadata.stack);
            return _a;
          })
        : [];

      types[typename].fields.push(_f);
    });

    parentTypes.forEach(t => {
      handleType(existingTypemap[t], prefixTypename(t));
    });
  };

  const handleInputObjectType = (type, typename) => {
    const _type = {
      name: typename,
      kind: 'input_object',
      fields: [],
      description: getEntityDescription(type),
    };

    const parentTypes = [];

    Object.values(type._fields).forEach(f => {
      const _f = {
        name: f.name,
        description: getEntityDescription(f),
      };

      const fieldTypeMetadata = getSchemaTypeMetadata(f.type);
      let prefixdTypename = fieldTypeMetadata.typename;
      if (!isInbuiltType(fieldTypeMetadata.typename)) {
        prefixdTypename = prefixTypename(fieldTypeMetadata.typename);
        parentTypes.push(fieldTypeMetadata.typename);
      }

      _f.type = wrapTypename(prefixdTypename, fieldTypeMetadata.stack);
      _type.fields.push(_f);
    });

    types[typename] = _type;

    parentTypes.forEach(t => {
      handleType(existingTypemap[t], prefixTypename(t));
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
    prefixTypename(selectedExistingType)
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

    if (type && type.fields) {
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

  if (actionArgs.length) {
    actionArgs.forEach(a => {
      getDependentTypes(a.type);
    });
  }

  getDependentTypes(actionOutputType);

  return Object.values(actionTypes);
};

export const getOverlappingTypeConfirmation = (
  currentActionName,
  allActions,
  allTypes,
  overlappingTypenames
) => {
  const otherActions = allActions.filter(a => a.name !== currentActionName);

  const typeCollisionMap = {};

  for (let i = otherActions.length - 1; i >= 0; i--) {
    const action = otherActions[i];
    const actionTypes = getActionTypes(action, allTypes);
    actionTypes.forEach(t => {
      if (!t || typeCollisionMap[t.name]) return;
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
