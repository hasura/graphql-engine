import { camelize } from 'inflection';
import { parse as sdlParse } from 'graphql/language/parser';

import { filterNameLessTypeLess, filterNameless } from '../../Types/utils';
import { getTypenameMetadata } from '../../Types/wrappingTypeUtils';
import { isInputObjectType, isEnumType, isScalarType } from 'graphql';
import {
  gqlInbuiltTypes,
  defaultScalarType,
  defaultField,
} from './stateDefaults';

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
  currentTypename,
  actionTypes,
  selectedExistingType,
  existingTypemap
) => {
  const currentTypeIndex = actionTypes.findIndex(
    at => at.name === currentTypename
  );

  const namespaceTypename = name => {
    if (name === selectedExistingType) {
      return currentTypename;
    }
    return camelize(`${currentTypename}_${name}`);
  };

  const getTypeKind = graphqlType => {
    if (isInputObjectType(graphqlType)) return 'input_object';
    if (isEnumType(graphqlType)) return 'enum';
    if (isScalarType(graphqlType)) return 'scalar';
  };

  let newTypes = filterNameless(actionTypes);

  const chosenExistingType = existingTypemap[selectedExistingType];

  const usedTypenames = {};

  const isInbuiltType = _t => {
    return gqlInbuiltTypes.find(it => it.name === _t.name);
  };

  const generateTypes = (selectedType, enforcedTypename) => {
    if (isInbuiltType(selectedType)) return;

    const typeKind = getTypeKind(selectedType);

    const _tname = enforcedTypename || namespaceTypename(selectedType.name);
    if (usedTypenames[_tname]) return;
    usedTypenames[_tname] = true;
    const _t = {
      name: _tname,
      kind: typeKind,
    };

    if (typeKind === 'enum') {
      newTypes.push({
        ..._t,
        values: selectedType._values,
      });
      return;
    }

    if (typeKind !== 'input_object') {
      newTypes.push(_t);
      return;
    }

    _t.fields = [];

    Object.values(selectedType._fields).forEach(f => {
      const _f = {
        name: f.name,
      };
      const { index: typeWrap, typename } = getTypenameMetadata(f.type);
      generateTypes(existingTypemap[typename]);
      _f.typeWrap = typeWrap.toString();
      const namespacedTypename = isInbuiltType({ name: typename })
        ? typename
        : namespaceTypename(typename);
      _f.typename = namespacedTypename;
      _t.fields.push(_f);
    });

    _t.fields.push({
      ...defaultField,
    });

    if (enforcedTypename) {
      newTypes[currentTypeIndex] = _t;
    } else {
      newTypes.push(_t);
    }
  };

  generateTypes(chosenExistingType, currentTypename);

  newTypes = newTypes.map(nt => {
    if (nt.kind === 'input_object') {
      const newType = JSON.parse(JSON.stringify(nt));
      newType.fields = newType.fields.map(f => {
        const _f = { ...f };
        _f.type = newTypes.findIndex(t => t.name === f.typename).toString();
        delete _f.typename;
        return _f;
      });
      return newType;
    }
    return nt;
  });

  return [...newTypes, defaultScalarType];
};

const getWrappedTypeNameFromAst = type => {
  let _t = { type };
  const typewraps = [];
  while (_t.kind !== 'NamedType') {
    if (_t.kind === 'ListType') {
      typewraps.push('l');
    }
    if (_t.kind === 'NonNullType') {
      typewraps.push('n');
    }
    _t = _t.type;
  }
  let typename = _t.name.value;
  typewraps.forEach(w => {
    if (w === 'l') {
      typename = `[${typename}]`;
    }
    if (w === 'n') {
      typename = `${typename}!`;
    }
  });

  return typename;
};

export const getTypesFromSdl = sdl => {
  const typeDefinition = {
    types: [],
    error: null,
  };

  if (!sdl) {
    return typeDefinition;
  }

  const schemaAst = sdlParse(sdl);

  const handleScalar = def => {
    typeDefinition.types.push({
      name: def.name.value,
      kind: 'scalar',
    });
  };

  const handleEnum = def => {
    typeDefinition.types.push({
      name: def.name.value,
      kind: 'enum',
      values: def.values.map(v => ({
        value: v.name.value,
        description: v.description,
      })),
    });
  };

  const handleInputObject = def => {
    typeDefinition.types.push({
      name: def.name.value,
      kind: 'input_object',
      fields: def.fields.map(f => ({
        name: f.name.value,
        type: getWrappedTypeNameFromAst(f.type),
      })),
    });
  };

  const handleObject = def => {
    typeDefinition.types.push({
      name: def.name.value,
      kind: 'object',
      fields: def.fields.map(f => ({
        name: f.name.value,
        type: getWrappedTypeNameFromAst(f.type),
      })),
    });
  };

  schemaAst.definitions.forEach(def => {
    switch (def.kind) {
      case 'ScalarTypeDefinition':
        handleScalar(def);
        return;
      case 'EnumTypeDefinition':
        handleEnum(def);
        return;
      case 'InputObjectTypeDefinition':
        handleInputObject(def);
        return;
      case 'ObjectTypeDefinition':
        handleObject(def);
        return;
      case 'SchemaDefinition':
        typeDefinition.error =
          'You cannot have schema definitions in Action/Type definitions';
        return;
      case 'InterfaceTypeDefinition':
        typeDefinition.error = 'Interface types are not supported';
        return;
      default:
        return;
    }
  });

  console.log(typeDefinition);

  return typeDefinition;
};

export const getSdlFromDef = () => {};

export const getActionDefinitionFromSdl = sdl => {
  const schemaAst = sdlParse(sdl);
  const definition = {
    name: '',
    arguments: [],
    outputType: '',
    error: null,
  };
  if (schemaAst.definitions.length > 1) {
    definition.error = 'Action must be defined under a single "Mutation" type';
    return definition;
  }

  const sdlDef = schemaAst.definitions[0];
  if (
    sdlDef.kind !== 'ObjectTypeDefinition' ||
    sdlDef.name.value !== 'Mutation'
  ) {
    definition.error = 'Action must be defined under a "Mutation" type';
    return definition;
  }

  if (sdlDef.fields.length > 1) {
    const definedActions = sdlDef.fields
      .map(f => `"${f.name.value}"`)
      .join(', ');
    definition.error = `You have defined multiple actions (${definedActions}). Please define only one.`;
    return definition;
  }

  const actionDef = sdlDef.fields[0];

  definition.name = actionDef.name.value;
  definition.outputType = getWrappedTypeNameFromAst(actionDef.type);
  definition.arguments = actionDef.arguments.map(a => {
    return {
      name: a.name.value,
      type: getWrappedTypeNameFromAst(a.type),
      description: a.description,
    };
  });

  console.log(definition);

  return definition;
};
