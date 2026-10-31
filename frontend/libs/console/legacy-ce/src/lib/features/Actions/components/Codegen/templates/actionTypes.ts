import {
  FieldDefinitionNode,
  InputValueDefinitionNode,
  Kind,
  OperationDefinitionNode,
  TypeNode,
  parse,
} from 'graphql';
import { codeBlock } from './codeBlock';

// View of the actions SDL shared by the typescript-express,
// javascript-js-doc-express and go-serve-mux templates.

export type FieldInfo = {
  name: string;
  type: TypeNode;
};

export type TypeMap = {
  // object and input object types, plus the root types and an `<action>Args`
  // type holding the arguments of each root field
  types: Record<string, FieldInfo[]>;
  enums: Record<string, string[]>;
  // custom scalars, including types that are referenced but never defined
  scalars: string[];
};

const ROOT_TYPES = ['Query', 'Mutation'];

const BUILTIN_SCALARS = ['ID', 'Int', 'Float', 'String', 'Boolean'];

export const isBuiltinScalar = (typename: string) =>
  BUILTIN_SCALARS.includes(typename);

const SAMPLE_VALUES: Record<string, string> = {
  ID: '1111',
  Int: '1111',
  Float: '11.11',
  String: '"<sample value>"',
  Boolean: 'false',
};

// placeholder value of a scalar in the generated handler
export const getSampleValue = (typename: string) =>
  SAMPLE_VALUES[typename] ?? SAMPLE_VALUES.String;

export const capitalize = (s: string) =>
  s.length ? s[0].toUpperCase() + s.substring(1) : s;

// the innermost named type, e.g. `Int` for `[[Int!]]`
export const getNamedType = (type: TypeNode): string =>
  type.kind === Kind.NAMED_TYPE ? type.name.value : getNamedType(type.type);

export const isListType = (type: TypeNode): boolean =>
  type.kind === Kind.NON_NULL_TYPE
    ? isListType(type.type)
    : type.kind === Kind.LIST_TYPE;

const toFieldInfo = (
  node: FieldDefinitionNode | InputValueDefinitionNode,
): FieldInfo => ({ name: node.name.value, type: node.type });

export const buildActionTypes = (actionName: string, actionsSdl: string) => {
  const ast = parse(actionsSdl);
  const typeMap: TypeMap = { types: {}, enums: {}, scalars: [] };
  const rootFields: Record<string, FieldDefinitionNode[]> = {
    Query: [],
    Mutation: [],
  };

  ast.definitions.forEach((def) => {
    switch (def.kind) {
      case Kind.SCALAR_TYPE_DEFINITION:
        typeMap.scalars.push(def.name.value);
        break;
      case Kind.ENUM_TYPE_DEFINITION:
        typeMap.enums[def.name.value] = (def.values ?? []).map(
          (v) => v.name.value,
        );
        break;
      case Kind.INPUT_OBJECT_TYPE_DEFINITION:
        typeMap.types[def.name.value] = (def.fields ?? []).map(toFieldInfo);
        break;
      case Kind.OBJECT_TYPE_DEFINITION:
      case Kind.OBJECT_TYPE_EXTENSION:
        if (ROOT_TYPES.includes(def.name.value)) {
          rootFields[def.name.value].push(...(def.fields ?? []));
        } else if (def.kind === Kind.OBJECT_TYPE_DEFINITION) {
          typeMap.types[def.name.value] = (def.fields ?? []).map(toFieldInfo);
        }
        break;
      default:
        break;
    }
  });

  ROOT_TYPES.forEach((root) => {
    if (rootFields[root].length) {
      typeMap.types[root] = rootFields[root].map(toFieldInfo);
    }
  });
  ROOT_TYPES.forEach((root) => {
    rootFields[root].forEach((field) => {
      typeMap.types[`${field.name.value}Args`] = (field.arguments ?? []).map(
        toFieldInfo,
      );
    });
  });

  Object.values(typeMap.types).forEach((fields) => {
    fields.forEach(({ type }) => {
      const typename = getNamedType(type);
      if (
        !isBuiltinScalar(typename) &&
        !(typename in typeMap.types) &&
        !(typename in typeMap.enums) &&
        !typeMap.scalars.includes(typename)
      ) {
        typeMap.scalars.push(typename);
      }
    });
  });

  const actionField = [...rootFields.Query, ...rootFields.Mutation].find(
    (f) => f.name.value === actionName,
  );
  if (!actionField) {
    throw new Error(`Action "${actionName}" not found in the actions SDL`);
  }

  return { actionOutput: actionField.type, typeMap };
};

// name (or alias) of the first root field selected by a GraphQL operation
export const getRootFieldName = (operation: string, capitalized = false) => {
  try {
    const selection = (
      parse(operation).definitions[0] as OperationDefinitionNode
    ).selectionSet.selections[0];
    if (selection.kind !== Kind.FIELD) {
      return '';
    }
    const name = selection.alias ? selection.alias.value : selection.name.value;
    return capitalized ? capitalize(name) : name;
  } catch (e) {
    console.error('Got error in getRootFieldName:', e);
    return '';
  }
};

export const DEFAULT_HASURA_ENDPOINT = 'http://localhost:8080/v1/graphql';

// JavaScript placeholder value of an action output: an object with a
// placeholder per field, a scalar or enum value, or a list of one of these
export const getSampleOutput = (type: TypeNode, typeMap: TypeMap): string => {
  if (type.kind === Kind.NON_NULL_TYPE) {
    return getSampleOutput(type.type, typeMap);
  }
  if (type.kind === Kind.LIST_TYPE) {
    return codeBlock`
      [
        ${getSampleOutput(type.type, typeMap)},
      ]`;
  }

  const fields = typeMap.types[type.name.value];
  if (!fields) {
    return getSampleValue(type.name.value);
  }
  if (!fields.length) {
    return '{}';
  }
  const fieldValues = fields.map(
    (f) =>
      `${f.name}: ${isListType(f.type) ? '[]' : getSampleValue(getNamedType(f.type))}`,
  );
  return codeBlock`
    {
      ${fieldValues.join(',\n')},
    }`;
};
