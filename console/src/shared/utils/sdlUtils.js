import { parse as sdlParse } from 'graphql/language/parser';
import { getAstTypeMetadata, wrapTypename } from './wrappingTypeUtils';
import {
  reformCustomTypes,
  parseCustomTypes,
  hydrateTypeRelationships,
} from './hasuraCustomTypeUtils';

export const getTypeFromAstDef = astDef => {
  const handleScalar = def => {
    return {
      name: def.name.value,
      kind: 'scalar',
    };
  };

  const handleEnum = def => {
    return {
      name: def.name.value,
      kind: 'enum',
      values: def.values.map(v => ({
        value: v.name.value,
        description: v.description,
      })),
    };
  };

  const handleInputObject = def => {
    return {
      name: def.name.value,
      kind: 'input_object',
      fields: def.fields.map(f => {
        const fieldTypeMetadata = getAstTypeMetadata(f.type);
        return {
          name: f.name.value,
          type: wrapTypename(
            fieldTypeMetadata.typename,
            fieldTypeMetadata.stack
          ),
        };
      }),
    };
  };

  const handleObject = def => {
    return {
      name: def.name.value,
      kind: 'object',
      fields: def.fields.map(f => {
        const fieldTypeMetadata = getAstTypeMetadata(f.type);
        return {
          name: f.name.value,
          type: wrapTypename(
            fieldTypeMetadata.typename,
            fieldTypeMetadata.stack
          ),
        };
      }),
    };
  };

  switch (astDef.kind) {
    case 'ScalarTypeDefinition':
      return handleScalar(astDef);
    case 'EnumTypeDefinition':
      return handleEnum(astDef);
    case 'InputObjectTypeDefinition':
      return handleInputObject(astDef);
    case 'ObjectTypeDefinition':
      return handleObject(astDef);
    case 'SchemaDefinition':
      return {
        error: 'You cannot have schema definitions in Action/Type definitions',
      };
    case 'InterfaceTypeDefinition':
      return {
        error: 'Interface types are not supported',
      };
    default:
      return;
  }
};

export const getTypesFromSdl = sdl => {
  const typeDefinition = {
    types: [],
    error: null,
  };

  if (!sdl || (sdl && sdl.trim() === '')) {
    return typeDefinition;
  }

  const schemaAst = sdlParse(sdl);

  schemaAst.definitions.forEach(def => {
    const typeDef = getTypeFromAstDef(def);
    typeDefinition.error = typeDef.error;
    typeDefinition.types.push(typeDef);
  });

  return typeDefinition;
};

const getActionFromMutationAstDef = astDef => {
  const definition = {
    name: '',
    arguments: [],
    outputType: '',
    error: null,
  };

  const actionDef = astDef.fields[0];

  definition.name = actionDef.name.value;
  const outputTypeMetadata = getAstTypeMetadata(actionDef.type);
  definition.outputType = wrapTypename(
    outputTypeMetadata.typename,
    outputTypeMetadata.stack
  );
  definition.arguments = actionDef.arguments.map(a => {
    const argTypeMetadata = getAstTypeMetadata(a.type);
    return {
      name: a.name.value,
      type: wrapTypename(argTypeMetadata.typename, argTypeMetadata.stack),
      description: a.description,
    };
  });

  return definition;
};

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

  if (sdlDef.name.value !== 'Mutation') {
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

  return {
    ...definition,
    ...getActionFromMutationAstDef(sdlDef),
  };
};

const getArgumentsSdl = args => {
  if (!args.length) return '';

  const argsSdl = args.map(a => {
    return `    ${a.name}: ${a.type}`;
  });

  return `(\n${argsSdl.join('\n')}\n  )`;
};

const getFieldsSdl = fields => {
  const fieldsSdl = fields.map(f => {
    const argSdl = f.arguments ? getArgumentsSdl(f.arguments) : '';
    return `  ${f.name} ${argSdl}: ${f.type}`;
  });
  return fieldsSdl.join('\n');
};

const getObjectTypeSdl = type => {
  return `type ${type.name} {
${getFieldsSdl(type.fields)}
}\n\n`;
};

const getInputTypeSdl = type => {
  return `input ${type.name} {
${getFieldsSdl(type.fields)}
}\n\n`;
};

const getScalarTypeSdl = type => {
  return `scalar ${type.name}\n\n`;
};

const getEnumTypeSdl = type => {
  const enumValuesSdl = type.values.map(v => {
    return `  ${v.value}`;
  });
  return `enum ${type.name} {
${enumValuesSdl.join('\n')}
}\n\n`;
};

const getTypeSdl = type => {
  switch (type.kind) {
    case 'scalar':
      return getScalarTypeSdl(type);
    case 'enum':
      return getEnumTypeSdl(type);
    case 'input_object':
      return getInputTypeSdl(type);
    case 'object':
      return getObjectTypeSdl(type);
    default:
      return '';
  }
};

export const getTypesSdl = _types => {
  let types = _types;
  if (types.constructor.name !== 'Array') {
    types = parseCustomTypes(_types);
  }
  let sdl = '';
  types.forEach(t => {
    sdl += getTypeSdl(t);
  });
  return sdl;
};

export const getActionDefinitionSdl = (name, args, outputType) => {
  return getObjectTypeSdl({
    name: 'Mutation',
    fields: [
      {
        name,
        arguments: args,
        type: outputType,
      },
    ],
  });
};

export const getServerTypesFromSdl = (sdl, existingTypes) => {
  const { types: typesFromSdl, error } = getTypesFromSdl(sdl);
  return {
    types: reformCustomTypes(
      hydrateTypeRelationships(typesFromSdl, parseCustomTypes(existingTypes))
    ),
    error,
  };
};

export const getAllActionsFromSdl = sdl => {
  const ast = sdlParse(sdl);
  ast.definitions = ast.definitions.filter(d => d.name.value === 'Mutation');
  const actions = ast.definitions.map(d => {
    const action = getActionFromMutationAstDef(d);
    return {
      name: action.name,
      definition: {
        arguments: action.arguments,
        output_type: action.outputType,
      },
    };
  });
  return actions;
};

export const getAllTypesFromSdl = sdl => {
  const ast = sdlParse(sdl);
  ast.definitions = ast.definitions.filter(d => d.name.value !== 'Mutation');
  const types = ast.definitions.map(d => {
    return getTypeFromAstDef(d);
  });
  return reformCustomTypes(types);
};
