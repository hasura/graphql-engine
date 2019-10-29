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
  if (wrappedTypename.includes('!]!')) {
    return {
      index: 5,
      typename: unwrapNullable(unwrapList(unwrapNullable(wrappedTypename))),
    };
  }
  if (wrappedTypename.includes(']!')) {
    return {
      index: 4,
      typename: unwrapList(unwrapNullable(wrappedTypename)),
    };
  }
  if (wrappedTypename.includes('!]')) {
    return {
      index: 3,
      typename: unwrapNullable(unwrapList(wrappedTypename)),
    };
  }
  if (wrappedTypename.includes(']')) {
    return {
      index: 2,
      typename: unwrapList(wrappedTypename),
    };
  }
  if (wrappedTypename.includes('!')) {
    return {
      index: 1,
      typename: unwrapNullable(wrappedTypename),
    };
  }
  return {
    index: 0,
    typename: wrappedTypename,
  };
};
