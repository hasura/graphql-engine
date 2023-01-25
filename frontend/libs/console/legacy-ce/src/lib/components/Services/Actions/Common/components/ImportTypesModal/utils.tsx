import z from 'zod';
import {
  GraphQLField,
  GraphQLObjectType,
  GraphQLSchema,
  isListType,
  isNonNullType,
  isObjectType,
} from 'graphql';

const TYPE_PREFIX = '_ActionType';
const INPUT_TYPE_PREFIX = '_ActionInputType';

export const schema = z.object({
  selectedTypes: z.array(z.string()),
  typeDef: z.string(),
});

export const editorOptions = {
  minLines: 22,
  maxLines: 22,
  showLineNumbers: true,
  useSoftTabs: true,
};

export type SchemaType = z.infer<typeof schema>;

export const addImportedTypeSuffixIfNotPresent = (
  type: string,
  io_type: 'input' | 'type'
) => {
  if (!type?.endsWith(TYPE_PREFIX)) {
    if (io_type === 'type') type = `${type}${TYPE_PREFIX}`;
    else type = `${type}${INPUT_TYPE_PREFIX}`;
  }
  return type;
};

export const getInnerType = (
  node: GraphQLField<any, any, { [key: string]: any }>
) => {
  let type = node.type;
  // get exact types - remove ! and []
  // ie. Int! => Int
  // [Int] => Int
  if (isNonNullType(node.type) || isListType(node.type)) {
    type = node.type.ofType;
    if ((isNonNullType(type) || isListType(type)) && type.ofType) {
      type = type.ofType;
      if ((isNonNullType(type) || isListType(type)) && type.ofType) {
        type = type.ofType; // the maximum it can go is 3 levels, an example is if both the list and inner type is non null [Track!]!
      }
    }
  }
  return type;
};

export const generateTypeDef = (
  clientSchema: GraphQLSchema,
  typeName: string,
  io_type: 'input' | 'type'
) => {
  let typeDef = `${io_type} ${typeName}${
    io_type === 'type' ? TYPE_PREFIX : INPUT_TYPE_PREFIX
  } {`;
  Object.entries(
    (clientSchema.getType(typeName) as GraphQLObjectType).getFields()
  ).forEach(([tName, tValue]) => {
    // parse nonNull or List types to find the inner type
    // Int! -> Int  or  [Int] -> Int
    const innerType = getInnerType(tValue);

    // if Object Type, suffix `_ActionType` to the typeName
    if (isObjectType(innerType)) {
      innerType.name = addImportedTypeSuffixIfNotPresent(
        innerType.name,
        io_type
      );
    }

    // if Scalar Type, re-use the typeName
    else {
      typeDef = `${typeDef}
  ${tName} : ${tValue.type.toString()}`;
    }
  });
  typeDef = `${typeDef}
}`;
  return typeDef;
};

export const generateAllTypeDefinitions = (
  clientSchema: GraphQLSchema,
  selectedTypes: string[],
  io_type: 'input' | 'type'
) => {
  const typeMap = clientSchema.getTypeMap();
  const allRequiredTypes = new Set([...selectedTypes]);

  let allTypeDefs = '';

  Object.entries(typeMap).forEach(([key, value]) => {
    // get types only that are generated from table.
    // this is based on the assumption that the table types will have the description that starts with  `columns and relationships of `
    if (
      value?.description?.startsWith('columns and relationships of ') &&
      clientSchema.getType(key) instanceof GraphQLObjectType
    ) {
      if (!allRequiredTypes.has(key)) return;
      const typeDef = generateTypeDef(clientSchema, key, io_type);
      allTypeDefs = `${allTypeDefs}${typeDef}
`;
    }
  });
  return allTypeDefs;
};

export const getAllTypeNames = (
  clientSchema: GraphQLSchema,
  onlyTableTypes = false
) => {
  // This works only for Hasura GraphQL Schema
  // the filtering logic is based on the generated description from the table type.
  if (onlyTableTypes) {
    return Object.entries(clientSchema.getTypeMap())
      .filter(([tName, tValue]) => {
        if (
          tValue?.description?.startsWith('columns and relationships of ') &&
          clientSchema.getType(tName) instanceof GraphQLObjectType
        )
          return true;
        return false;
      })
      .map(i => i[0]);
  }
  return Object.keys(clientSchema.getTypeMap());
};
