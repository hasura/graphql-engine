const { unwrapType } = require('./wrappingTypeUtils');

const inbuiltTypes = {
  'Int': true,
  'Boolean': true,
  'String': true,
  'Float': true
};

const singularize = kind => {
  return kind.substr(0, kind.length - 1);
};

const pluralize = kind => {
  return `${kind}s`;
}

const filterNameLessTypeLess = arr => {
  return arr.filter(item => !!item.name && !!item.type);
};

const filterNameless = arr => {
  return arr.filter(item => !!item.name);
};

const filterValueLess = arr => {
  return arr.filter(item => !!item.value);
};

const reformCustomTypes = typesFromState => {
  const sanitisedTypes = [];
  typesFromState.forEach(t => {
    if (!t.name) {
      return;
    }
    const sanitisedType = { ...t };
    if (t.fields) {
      sanitisedType.fields = filterNameLessTypeLess(t.fields);
    }
    if (t.arguments) {
      sanitisedType.arguments = filterNameLessTypeLess(t.arguments);
    }

    sanitisedTypes.push(sanitisedType);
  });

  const customTypes = {
    scalars: [],
    input_objects: [],
    objects: [],
    enums: [],
  };

  sanitisedTypes.forEach(_type => {
    const type = JSON.parse(JSON.stringify(_type));
    delete type.kind;
    customTypes[pluralize(_type.kind)].push(type);
  });

  return customTypes;

};

const parseCustomTypes = customTypesServer => {
  const customTypesClient = [];
  Object.keys(customTypesServer).forEach(tk => {
    const types = customTypesServer[tk];
    types.forEach(t => {
      customTypesClient.push({
        ...t,
        kind: singularize(tk),
      });
    });
  });
  return customTypesClient;
};

const hydrateTypeRelationships = (newTypes, existingTypes) => {
  const typeMap = {};
  existingTypes.forEach(t => {
    typeMap[t.name] = t;
  });

  // TODO

  return newTypes.map(t => {
    if (t.kind === 'object' && typeMap[t.name]) {
      return {
        ...t,
        relationships: typeMap[t.name].relationships
      };
    }

    return t;
  });
};

const getActionTypes = (actionDef, allTypes) => {
  const usedTypes = {};
  const actionTypes = [];

  const getDependentTypes = typename => {
    if (usedTypes[typename]) return;
    const type = allTypes.find(t => t.name === typename);
    if (!type) return;
    actionTypes.push(type);
    usedTypes[typename] = true;
    if (type.kind === 'input_object' || type.kind === 'object') {
      type.fields.forEach(f => {
        const { typename: _typename } = unwrapType(f.type);
        getDependentTypes(_typename);
      });
    }
  };

  actionDef.arguments.forEach(a => {
    const { typename } = unwrapType(a.type);
    getDependentTypes(typename);
  });

  getDependentTypes(actionDef.output_type);

  return actionTypes;
};

module.exports = {
  hydrateTypeRelationships,
  reformCustomTypes,
  parseCustomTypes,
  getActionTypes,
  inbuiltTypes 
};