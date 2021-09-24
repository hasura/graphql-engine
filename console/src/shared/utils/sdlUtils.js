import { parse as sdlParse } from 'graphql/language/parser';
import { getAstTypeMetadata, wrapTypename } from './wrappingTypeUtils';
import {
  reformCustomTypes,
  parseCustomTypes,
  hydrateTypeRelationships,
} from './hasuraCustomTypeUtils';

export const isValidOperationName = operationName => {
  return operationName === 'query' || operationName === 'mutation';
};

const isValidOperationType = operationType => {
  return operationType === 'Mutation' || operationType === 'Query';
};

const getActionTypeFromOperationType = operationType => {
  if (operationType === 'Query') {
    return 'query';
  }
  return 'mutation';
};

const getOperationTypeFromActionType = operationType => {
  if (operationType === 'query') {
    return 'Query';
  }
  return 'Mutation';
};

const getAstEntityDescription = def => {
  return def.description ? def.description.value.trim() : null;
};

const getEntityDescriptionSdl = def => {
  let entityDescription = def.description;
  entityDescription = entityDescription ? `""" ${entityDescription} """ ` : '';
  return entityDescription;
};

export const getTypeFromAstDef = astDef => {
  const handleScalar = def => {
    return {
      name: def.name.value,
      description: getAstEntityDescription(def),
      kind: 'scalar',
    };
  };

  const handleEnum = def => {
    return {
      name: def.name.value,
      kind: 'enum',
      description: getAstEntityDescription(def),
      values: def.values.map(v => ({
        value: v.name.value,
        description: getAstEntityDescription(v),
      })),
    };
  };

  const handleInputObject = def => {
    return {
      name: def.name.value,
      kind: 'input_object',
      description: getAstEntityDescription(def),
      fields: def.fields.map(f => {
        const fieldTypeMetadata = getAstTypeMetadata(f.type);
        return {
          name: f.name.value,
          type: wrapTypename(
            fieldTypeMetadata.typename,
            fieldTypeMetadata.stack
          ),
          description: getAstEntityDescription(f),
        };
      }),
    };
  };

  const handleObject = def => {
    return {
      name: def.name.value,
      kind: 'object',
      description: getAstEntityDescription(def),
      fields: def.fields.map(f => {
        const fieldTypeMetadata = getAstTypeMetadata(f.type);
        return {
          name: f.name.value,
          type: wrapTypename(
            fieldTypeMetadata.typename,
            fieldTypeMetadata.stack
          ),
          description: getAstEntityDescription(f),
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

const getActionFromOperationAstDef = astDef => {
  const definition = {
    name: '',
    arguments: [],
    outputType: '',
    comment: getAstEntityDescription(astDef),
    error: null,
  };

  definition.name = astDef.name.value;
  const outputTypeMetadata = getAstTypeMetadata(astDef.type);
  definition.outputType = wrapTypename(
    outputTypeMetadata.typename,
    outputTypeMetadata.stack
  );
  definition.arguments = astDef.arguments.map(a => {
    const argTypeMetadata = getAstTypeMetadata(a.type);
    return {
      name: a.name.value,
      type: wrapTypename(argTypeMetadata.typename, argTypeMetadata.stack),
      description: getAstEntityDescription(a),
    };
  });

  return definition;
};

export const getActionDefinitionFromSdl = sdl => {
  const definition = {
    name: '',
    arguments: [],
    outputType: '',
    comment: '',
    error: null,
  };
  let schemaAst;
  try {
    schemaAst = sdlParse(sdl);
  } catch {
    definition.error = 'Invalid SDL';
    return definition;
  }

  if (schemaAst.definitions.length > 1) {
    definition.error =
      'Action must be defined under a single "Mutation" type or a "Query" type';
    return definition;
  }

  const sdlDef = schemaAst.definitions[0];

  if (!isValidOperationType(sdlDef.name.value)) {
    definition.error =
      'Action must be defined under a "Mutation" or a "Query" type';
    return definition;
  }

  const actionType = getActionTypeFromOperationType(sdlDef.name.value);

  if (sdlDef.fields.length > 1) {
    const definedActions = sdlDef.fields
      .map(f => `"${f.name.value}"`)
      .join(', ');
    definition.error = `You have defined multiple actions (${definedActions}). Please define only one.`;
    return definition;
  }

  return {
    ...definition,
    type: actionType,
    ...getActionFromOperationAstDef(sdlDef.fields[0]),
  };
};

const getArgumentsSdl = args => {
  if (!args.length) return '';

  const argsSdl = args.map(a => {
    return `    ${getEntityDescriptionSdl(a)}${a.name}: ${a.type}`;
  });

  return `(\n${argsSdl.join('\n')}\n  )`;
};

const getFieldsSdl = fields => {
  const fieldsSdl = fields.map(f => {
    const argSdl = f.arguments ? getArgumentsSdl(f.arguments) : '';
    return `  ${getEntityDescriptionSdl(f)}${f.name}${argSdl}: ${f.type}`;
  });
  return fieldsSdl.join('\n');
};

const getObjectTypeSdl = type => {
  return `${getEntityDescriptionSdl(type)}type ${type.name} {
${getFieldsSdl(type.fields)}
}\n\n`;
};

const getInputTypeSdl = type => {
  return `${getEntityDescriptionSdl(type)}input ${type.name} {
${getFieldsSdl(type.fields)}
}\n\n`;
};

const getScalarTypeSdl = type => {
  return `${getEntityDescriptionSdl(type)}scalar ${type.name}\n\n`;
};

const getEnumTypeSdl = type => {
  const enumValuesSdl = type.values.map(v => {
    return `  ${getEntityDescriptionSdl(v)}${v.value}`;
  });
  return `${getEntityDescriptionSdl(type)}enum ${type.name} {
${enumValuesSdl.join('\n')}
}\n\n`;
};

const getTypeSdl = type => {
  if (!type) return '';
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

export const getActionDefinitionSdl = (
  name,
  actionType,
  args,
  outputType,
  description
) => {
  return getObjectTypeSdl({
    name: getOperationTypeFromActionType(actionType),
    fields: [
      {
        name,
        arguments: args,
        type: outputType,
        description,
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
  const actions = [];

  ast.definitions
    .filter(d => isValidOperationType(d.name.value))
    .forEach(d => {
      d.fields.forEach(f => {
        const action = getActionFromOperationAstDef(f);
        actions.push({
          name: action.name,
          definition: {
            type: getActionTypeFromOperationType(d.name.value),
            arguments: action.arguments,
            output_type: action.outputType,
          },
        });
      });
    });
  return actions;
};

export const getAllTypesFromSdl = sdl => {
  const ast = sdlParse(sdl);
  ast.definitions = ast.definitions.filter(
    d => !isValidOperationType(d.name.value)
  );
  const types = ast.definitions.map(d => {
    return getTypeFromAstDef(d);
  });
  return reformCustomTypes(types);
};

export const getSdlComplete = (allActions, allTypes) => {
  let sdl = '';
  allActions.forEach(a => {
    sdl += `extend ${getActionDefinitionSdl(
      a.name,
      a.definition.type,
      a.definition.arguments,
      a.definition.output_type,
      a.comment
    )}`;
  });

  sdl += getTypesSdl(allTypes);
  return sdl;
};
