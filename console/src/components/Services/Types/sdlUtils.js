import { parse as sdlParse } from 'graphql/language/parser';
import { getWrappedTypeNameFromAst } from './wrappingTypeUtils';

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
