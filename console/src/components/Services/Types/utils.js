const singularize = kind => {
  return kind.substr(0, kind.length - 1);
};

export const filterNameLessTypeLess = arr => {
  return arr.filter(item => !!item.name && item.type);
};

export const mergeCustomTypes = (newTypesList, existingTypesList) => {
  let mergedTypes = [];
  let overlappingTypename = null;
  newTypesList.forEach(nt => {
    if (existingTypesList.find(et => et.name === nt.name)) {
      overlappingTypename = nt.name;
    }
    mergedTypes.push(nt);
  });
  mergedTypes = [...mergedTypes, ...existingTypesList];
  return {
    types: mergedTypes,
    overlappingTypename,
  };
};

export const reformCustomTypes = typesFromState => {
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
    switch (_type.kind) {
      case 'scalar':
        customTypes.scalars.push(type);
        return;
      case 'object':
        customTypes.objects.push(type);
        return;
      case 'input_object':
        customTypes.input_objects.push(type);
        return;
      default:
        return;
    }
  });

  return customTypes;
};

export const parseCustomTypes = customTypesServer => {
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
