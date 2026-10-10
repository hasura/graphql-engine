import { parse as sdlParse } from 'graphql/language/parser';
import { getAstTypeMetadata, wrapTypename } from './wrappingTypeUtils';
import { FlattenCustomType, flattenCustomTypes } from './hasuraCustomTypeUtils';
import {
  DefinitionNode,
  EnumTypeDefinitionNode,
  EnumValueDefinitionNode,
  FieldDefinitionNode,
  InputObjectTypeDefinitionNode,
  InputValueDefinitionNode,
  Kind,
  ObjectTypeDefinitionNode,
  ScalarTypeDefinitionNode,
  print,
  parse,
} from 'graphql';
import type {
  Action,
  CustomTypes,
  EnumType,
  InputArgument,
  InputObjectType,
  ObjectType,
  ScalarType,
} from '@hasura/shared/types';

export const isValidOperationName = (operationName: string): boolean => {
  return operationName === 'query' || operationName === 'mutation';
};

const isValidOperationType = (operationType: string | undefined): boolean => {
  return (
    operationType !== undefined &&
    (operationType === 'Mutation' || operationType === 'Query')
  );
};

const getActionTypeFromOperationType = (operationType: string): string => {
  if (operationType === 'Query') {
    return 'query';
  }
  return 'mutation';
};

export const formatSdl = (sdl: string) => {
  const ast = parse(sdl);
  return print(ast);
};

const getOperationTypeFromActionType = (operationType: string) => {
  if (operationType === 'query') {
    return 'Query';
  }
  return 'Mutation';
};

const getAstEntityDescription = (
  def:
    | ScalarTypeDefinitionNode
    | EnumTypeDefinitionNode
    | InputObjectTypeDefinitionNode
    | ObjectTypeDefinitionNode
    | FieldDefinitionNode
    | EnumValueDefinitionNode
    | InputValueDefinitionNode,
) => {
  return def.description?.value?.trim();
};

const getEntityDescriptionSdl = ({ description }: { description?: string }) => {
  const entityDescription = description?.trim();
  return entityDescription ? `""" ${entityDescription} """ ` : '';
};

export const getTypeFromAstDef = (astDef: DefinitionNode) => {
  const handleScalar = (def: ScalarTypeDefinitionNode) => {
    return {
      name: def.name.value,
      description: getAstEntityDescription(def),
      kind: 'scalar',
    };
  };

  const handleEnum = (def: EnumTypeDefinitionNode) => {
    return {
      name: def.name.value,
      kind: 'enum',
      description: getAstEntityDescription(def),
      values:
        def.values?.map((v) => ({
          value: v.name.value,
          description: getAstEntityDescription(v),
        })) ?? [],
    };
  };

  const handleInputObject = (def: InputObjectTypeDefinitionNode) => {
    return {
      name: def.name.value,
      kind: 'input_object',
      description: getAstEntityDescription(def),
      fields:
        def.fields?.map((f) => {
          const fieldTypeMetadata = getAstTypeMetadata(f.type);
          return {
            name: f.name.value,
            type: wrapTypename(
              fieldTypeMetadata.typename,
              fieldTypeMetadata.stack,
            ),
            description: getAstEntityDescription(f),
          };
        }) ?? [],
    };
  };

  const handleObject = (def: ObjectTypeDefinitionNode) => {
    return {
      name: def.name.value,
      kind: 'object',
      description: getAstEntityDescription(def),
      fields:
        def.fields?.map((f) => {
          const fieldTypeMetadata = getAstTypeMetadata(f.type);
          return {
            name: f.name.value,
            type: wrapTypename(
              fieldTypeMetadata.typename,
              fieldTypeMetadata.stack,
            ),
            description: getAstEntityDescription(f),
          };
        }) ?? [],
    };
  };

  switch (astDef.kind) {
    case Kind.SCALAR_TYPE_DEFINITION:
      return handleScalar(astDef);
    case Kind.ENUM_TYPE_DEFINITION:
      return handleEnum(astDef);
    case Kind.INPUT_OBJECT_TYPE_DEFINITION:
      return handleInputObject(astDef);
    case Kind.OBJECT_TYPE_DEFINITION:
      return handleObject(astDef);
    case Kind.SCHEMA_DEFINITION:
      return {
        error: 'You cannot have schema definitions in Action/Type definitions',
      };
    case Kind.INTERFACE_TYPE_DEFINITION:
      return {
        error: 'Interface types are not supported',
      };
    default:
      return;
  }
};

export type GetTypeFromAstDefResult = ReturnType<typeof getTypeFromAstDef>;

const graphqlToActionScalar = (def: ScalarTypeDefinitionNode): ScalarType => {
  return {
    name: def.name.value,
    description: getAstEntityDescription(def),
  };
};

const graphqlToActionEnum = (def: EnumTypeDefinitionNode): EnumType => {
  return {
    name: def.name.value,
    description: getAstEntityDescription(def),
    values:
      def.values?.map((v) => ({
        value: v.name.value,
        description: getAstEntityDescription(v),
      })) ?? [],
  };
};

const graphqlToActionInputObject = (
  def: InputObjectTypeDefinitionNode,
): InputObjectType => {
  return {
    name: def.name.value,
    description: getAstEntityDescription(def),
    fields:
      def.fields?.map((f) => {
        const fieldTypeMetadata = getAstTypeMetadata(f.type);
        return {
          name: f.name.value,
          type: wrapTypename(
            fieldTypeMetadata.typename,
            fieldTypeMetadata.stack,
          ),
          description: getAstEntityDescription(f),
        };
      }) ?? [],
  };
};

const graphqlToActionObject = (def: ObjectTypeDefinitionNode): ObjectType => {
  return {
    name: def.name.value,
    description: getAstEntityDescription(def),
    fields:
      def.fields?.map((f) => {
        const fieldTypeMetadata = getAstTypeMetadata(f.type);
        return {
          name: f.name.value,
          type: wrapTypename(
            fieldTypeMetadata.typename,
            fieldTypeMetadata.stack,
          ),
          description: getAstEntityDescription(f),
        };
      }) ?? [],
  };
};

export const buildCustomTypesFromSDL = (sdl: string) => {
  const typeDefinition = {
    types: {
      enums: [],
      input_objects: [],
      objects: [],
      scalars: [],
    } as CustomTypes,
    error: null as string | null,
  };

  if (!sdl || (sdl && sdl.trim() === '')) {
    return typeDefinition;
  }

  const schemaAst = sdlParse(sdl);

  schemaAst.definitions.forEach((astDef) => {
    switch (astDef.kind) {
      case Kind.SCALAR_TYPE_DEFINITION:
        typeDefinition.types.scalars?.push(graphqlToActionScalar(astDef));
        return;
      case Kind.ENUM_TYPE_DEFINITION:
        typeDefinition.types.enums?.push(graphqlToActionEnum(astDef));
        return;
      case Kind.INPUT_OBJECT_TYPE_DEFINITION:
        typeDefinition.types.input_objects?.push(
          graphqlToActionInputObject(astDef),
        );
        return;
      case Kind.OBJECT_TYPE_DEFINITION:
        typeDefinition.types.objects?.push(graphqlToActionObject(astDef));
        return;
      case Kind.SCHEMA_DEFINITION:
        typeDefinition.error =
          'You cannot have schema definitions in Action/Type definitions';
        return;
      case Kind.INTERFACE_TYPE_DEFINITION:
        typeDefinition.error = 'Interface types are not supported';
        return;
      default:
        return;
    }
  });

  return typeDefinition;
};

export const getTypesFromSdl = (sdl: string) => {
  const typeDefinition = {
    types: [] as Record<string, any>[],
    error: null as string | null,
  };

  if (!sdl || (sdl && sdl.trim() === '')) {
    return typeDefinition;
  }

  const schemaAst = sdlParse(sdl);

  schemaAst.definitions.forEach((def) => {
    const typeDef = getTypeFromAstDef(def);
    if (typeDef) {
      if ('error' in typeDef) {
        typeDefinition.error = typeDef.error;
      }

      typeDefinition.types.push(typeDef);
    }
  });

  return typeDefinition;
};

const getActionFromOperationAstDef = (astDef) => {
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
    outputTypeMetadata.stack,
  );
  definition.arguments = astDef.arguments.map((a) => {
    const argTypeMetadata = getAstTypeMetadata(a.type);
    return {
      name: a.name.value,
      type: wrapTypename(argTypeMetadata.typename, argTypeMetadata.stack),
      description: getAstEntityDescription(a),
    };
  });

  return definition;
};

export const getActionDefinitionFromSdl = (sdl) => {
  const definition = {
    name: '',
    arguments: [],
    outputType: '',
    comment: '',
    error: null as string | null,
    type: '',
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
      .map((f) => `"${f.name.value}"`)
      .join(', ');
    definition.error = `You have defined multiple actions (${definedActions}). Please define only one.`;
    return definition;
  }

  const defObj = sdlDef.fields.length
    ? {
        ...definition,
        type: actionType,
        ...getActionFromOperationAstDef(sdlDef.fields[0]),
      }
    : { ...definition, type: actionType };

  return defObj;
};

const getArgumentsSdl = (args) => {
  if (!args.length) return '';

  const argsSdl = args.map((a) => {
    return `    ${getEntityDescriptionSdl(a)}${a.name}: ${a.type}`;
  });

  return `(\n${argsSdl.join('\n')}\n  )`;
};

const getFieldsSdl = (fields) => {
  const fieldsSdl = fields.map((f) => {
    const argSdl = f.arguments ? getArgumentsSdl(f.arguments) : '';
    return `  ${getEntityDescriptionSdl(f)}${f.name}${argSdl}: ${f.type}`;
  });

  return fieldsSdl.join('\n');
};

const getObjectTypeSdl = (type: ObjectType) => {
  return `${getEntityDescriptionSdl(type)}type ${type.name} {
${getFieldsSdl(type.fields)}
}\n\n`;
};

const getInputTypeSdl = (type: InputObjectType) => {
  return `${getEntityDescriptionSdl(type)}input ${type.name} {
${getFieldsSdl(type.fields)}
}\n\n`;
};

const getScalarTypeSdl = (type: ScalarType) => {
  return `${getEntityDescriptionSdl(type)}scalar ${type.name}\n\n`;
};

const getEnumTypeSdl = (enumType: EnumType) => {
  const enumValuesSdl = enumType.values.map((v) => {
    return `  ${getEntityDescriptionSdl(v)}${v.value}`;
  });

  return `${getEntityDescriptionSdl(enumType)}enum ${enumType.name} {
${enumValuesSdl.join('\n')}
}\n\n`;
};

const getTypeSdl = (type: FlattenCustomType) => {
  if (!type) return '';
  switch (type.kind) {
    case 'scalars':
      return getScalarTypeSdl(type.definition);
    case 'enums':
      return getEnumTypeSdl(type.definition);
    case 'input_objects':
      return getInputTypeSdl(type.definition);
    case 'objects':
      return getObjectTypeSdl(type.definition);
    default:
      return '';
  }
};

export const buildTypeSDL = (customTypes: CustomTypes): string => {
  return [
    customTypes.enums?.map(getEnumTypeSdl) ?? '',
    customTypes.scalars?.map(getScalarTypeSdl) ?? '',
    customTypes.input_objects?.map(getInputTypeSdl) ?? '',
    customTypes.objects?.map(getObjectTypeSdl) ?? '',
  ].join('');
};

export const getTypesSdl = (types: FlattenCustomType[]) => {
  return types.reduce((sdl, t) => {
    return sdl + getTypeSdl(t);
  }, '');
};

export const getActionDefinitionSdl = (
  name: string,
  actionType: string,
  args: InputArgument[] | undefined,
  outputType: string,
  description?: string | undefined,
): string => {
  const operationName = getOperationTypeFromActionType(actionType);

  // type Mutation {
  //   # Define your action here
  //   actionName (arg1: SampleInput!): SampleOutput
  // }
  const argSdl = args?.length ? getArgumentsSdl(args) : '';
  const fieldSdl = `  ${getEntityDescriptionSdl({ description })}${name}${argSdl}: ${outputType}`;
  const sdl = `type ${operationName} {
${fieldSdl}
}\n\n`;

  return formatSdl(sdl);
};

export const getAllActionsFromSdl = (sdl) => {
  const ast = sdlParse(sdl);
  const actions: Record<string, any>[] = [];

  ast.definitions
    .filter((d) => {
      d.kind === Kind.OPERATION_DEFINITION && d.name?.value;
    })
    .forEach((def) => {
      const d = def as Record<string, any>;
      d.fields.forEach((f) => {
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

export const getSdlComplete = async (
  allActions: Action[] | undefined,
  allTypes: CustomTypes | undefined,
): Promise<string> => {
  let sdl = '';

  if (allActions?.length) {
    sdl = allActions
      .map((a) => {
        const actionSdl = getActionDefinitionSdl(
          a.name,
          a.definition.type ?? 'Query',
          a.definition.arguments,
          a.definition.output_type,
          a.comment,
        );

        return `extend ${actionSdl}`;
      })
      .join('');
  }

  if (allTypes) {
    sdl += getTypesSdl(flattenCustomTypes(allTypes));
  }

  return sdl;
};
