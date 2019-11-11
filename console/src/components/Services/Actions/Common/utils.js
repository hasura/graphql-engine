/* eslint-disable no-use-before-define */

import { camelize } from 'inflection';
import { filterNameLessTypeLess } from '../../Types/utils';
import {
  getSchemaTypeMetadata,
  wrapTypename,
} from '../../Types/wrappingTypeUtils';
import {
  isInputObjectType,
  isEnumType,
  isScalarType,
  isObjectType,
} from 'graphql';

import { gqlInbuiltTypes } from './stateDefaults';

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
