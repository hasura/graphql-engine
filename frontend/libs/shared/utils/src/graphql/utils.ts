import {
  getNullableType,
  GraphQLType,
  isListType,
  isNonNullType,
  isObjectType,
  isWrappingType,
} from 'graphql';

export const getScalarType = (type: any): string => {
  if (type.kind === 'SCALAR') return type.name;

  return getScalarType(type.ofType);
};

export const getGraphQLUnderlyingType = (gqlType: GraphQLType) => {
  let result = gqlType;

  const wraps: string[] = [];

  while (isWrappingType(result)) {
    if (isListType(result)) {
      wraps.push('l');
      result = result.ofType;
    }

    if (isNonNullType(result)) {
      wraps.push('n');
      result = result.ofType;
    }
  }

  return {
    wraps,
    type: result,
  };
};

export function isScalarGraphQLType(graphQLType?: GraphQLType): boolean {
  if (!graphQLType) {
    return false;
  }
  const nullableType = getNullableType(graphQLType);
  const isObjectOrArray =
    isListType(nullableType) || isObjectType(nullableType);
  return !isObjectOrArray;
}
