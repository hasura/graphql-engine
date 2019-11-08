import { isWrappingType, isListType, isNonNullType } from 'graphql';

const wrapNonNullable = typename => {
  return `${typename}!`;
};

const wrapList = typename => {
  return `[${typename}]`;
};

const wrapNullableListOfNonNullable = typename => {
  return `[${typename}!]`;
};

const wrapNonNullableListOfNullable = typename => {
  return `[${typename}]!`;
};

const wrapNonNullableListOfNonNullable = typename => {
  return `[${typename}!]!`;
};

export const isNonNullable = wrappedTypename => {
  return wrappedTypename[wrappedTypename.length - 1] === '!';
};

export const isList = wrappedTypename => {
  return wrappedTypename[wrappedTypename.length - 1] === ']';
};

const unwrapNullable = wrappedTypename => {
  return wrappedTypename.substring(0, wrappedTypename.length - 1);
};

const unwrapList = wrappedTypename => {
  return wrappedTypename.substring(1, wrappedTypename.length - 1);
};

export const typeWrappers = [
  {
    label: 'Nullable',
    wrapperFunc: typename => typename,
  },
  {
    label: 'Non-nullable',
    wrapperFunc: wrapNonNullable,
  },
  {
    label: 'Nullable list of nullable',
    wrapperFunc: wrapList,
  },
  {
    label: 'Nullable list of non-nullable',
    wrapperFunc: wrapNullableListOfNonNullable,
  },
  {
    label: 'Non-nullable list of nullable',
    wrapperFunc: wrapNonNullableListOfNullable,
  },
  {
    label: 'Non-nullable list of non-nullable',
    wrapperFunc: wrapNonNullableListOfNonNullable,
  },
];

export const wrapType = (typename, wrapperIndex) => {
  return typeWrappers[wrapperIndex].wrapperFunc(typename);
};

export const unwrapType = wrappedTypename => {
  if (wrappedTypename.charAt(wrappedTypename.length - 1) === '!') {
    return unwrapType(unwrapNullable(wrappedTypename));
  }
  if (wrappedTypename.charAt(wrappedTypename.length - 1) === ']') {
    return unwrapType(unwrapList(wrappedTypename));
  }
  return {
    index: 0,
    typename: wrappedTypename,
  };
};

export const getTypenameMetadata = graphqlType => {
  let _type = graphqlType;
  const wrappers = [];

  while (isWrappingType(_type)) {
    if (isListType(_type)) {
      wrappers.push('l');
    }
    if (isNonNullType(_type)) {
      wrappers.push('n');
    }
    _type = _type.ofType;
  }

  const typename = _type.name;
  let wrappedTypename = typename;

  while (wrappers[0]) {
    const l = wrappers.length;
    if (wrappers[l - 1] === 'l') {
      wrappedTypename = `[${wrappedTypename}]`;
      wrappers.pop();
      continue;
    }
    if (wrappers[l - 1] === 'n') {
      wrappedTypename = `${wrappedTypename}!`;
      wrappers.pop();
      continue;
    }
  }

  return unwrapType(wrappedTypename);
};

export const getUnderlyingType = graphqlType => {
  let _type = graphqlType;
  while (isWrappingType(_type)) {
    _type = _type.ofType;
  }
  return _type;
};
