import {
  isWrappingType,
  isListType,
  isNonNullType,
  TypeNode,
  GraphQLObjectType,
  GraphQLInputObjectType,
} from 'graphql';

const unwrapNonNullable = (wrappedTypename: string) => {
  return wrappedTypename.substring(0, wrappedTypename.length - 1);
};

const unwrapList = (wrappedTypename: string) => {
  return wrappedTypename.substring(1, wrappedTypename.length - 1);
};

export const unwrapType = (wrappedTypename: string) => {
  let typename = wrappedTypename;
  const typeWrapperStack = [];
  let lastChar = typename.charAt(typename.length - 1);

  while (lastChar) {
    if (lastChar === ']') {
      typename = unwrapList(typename);
      typeWrapperStack.push('l');
    } else if (lastChar === '!') {
      typename = unwrapNonNullable(typename);
      typeWrapperStack.push('n');
    } else {
      break;
    }
    lastChar = typename.charAt(typename.length - 1);
  }

  return {
    stack: typeWrapperStack,
    typename,
  };
};

export const getAstTypeMetadata = (type: TypeNode) => {
  let node = { ...type };
  const typewraps = [];
  while (node.kind !== 'NamedType') {
    if (node.kind === 'ListType') {
      typewraps.push('l');
    }
    if (node.kind === 'NonNullType') {
      typewraps.push('n');
    }
    node = node.type;
  }
  const typename = node.name.value;
  return {
    typename,
    stack: typewraps,
  };
};

export const getSchemaTypeMetadata = (
  type: GraphQLObjectType | GraphQLInputObjectType
) => {
  let t = type;
  const typewraps = [];
  while (isWrappingType(t)) {
    if (isListType(t)) {
      typewraps.push('l');
    }
    if (isNonNullType(t)) {
      typewraps.push('n');
    }
    t = t.ofType;
  }

  return {
    typename: t.name,
    stack: typewraps,
  };
};

export const wrapTypename = (name: string, wrapperStack: string[]) => {
  let wrappedTypename = name;
  wrapperStack.reverse().forEach(w => {
    if (w === 'l') {
      wrappedTypename = `[${wrappedTypename}]`;
    }
    if (w === 'n') {
      wrappedTypename = `${wrappedTypename}!`;
    }
  });
  return wrappedTypename;
};
