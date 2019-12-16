const getAstTypeMetadata = type => {
  let _t = { type };
  const typewraps = [];
  while (_t.kind !== 'NamedType') {
    if (_t.kind === 'ListType') {
      typewraps.push('l');
    }
    if (_t.kind === 'NonNullType') {
      typewraps.push('n');
    }
    _t = _t.type;
  }
  const typename = _t.name.value;
  return {
    typename,
    stack: typewraps,
  };
};

const wrapTypename = (name, wrapperStack) => {
  let wrappedTypename = name;
  wrapperStack.forEach(w => {
    if (w === 'l') {
      wrappedTypename = `[${wrappedTypename}]`;
    }
    if (w === 'n') {
      wrappedTypename = `${wrappedTypename}!`;
    }
  });
  return wrappedTypename;
};

const unwrapList = (typename) => {
  return typename.substring(1, typename.length - 1);
};

const unwrapNonNullable = (typename) => {
  return typename.substring(0, typename.length - 1);
};

const unwrapType = wrappedTypename => {
  let _typename = wrappedTypename;
  const typeWrapperStack = [];

  while (true) {
    const _lastChar = _typename.charAt(_typename.length - 1);
    if (_lastChar === ']') {
      _typename = unwrapList(_typename);
      typeWrapperStack.push('l');
      continue;
    }
    if (_lastChar === '!') {
      _typename = unwrapNonNullable(_typename);
      typeWrapperStack.push('n');
      continue;
    }

    break;
  }

  return {
    stack: typeWrapperStack,
    typename: _typename,
  };
};

module.exports = {
  getAstTypeMetadata,
  wrapTypename,
  unwrapType 
}