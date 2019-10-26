export const defaultArg = {
  name: '',
  type: '',
  description: '',
};

export const defaultField = {
  name: '',
  type: '',
};

export const defaultScalarType = {
  name: '',
  kind: 'scalar',
};

export const defaultObjectType = {
  name: '',
  kind: 'object',
  fields: [{ ...defaultField }],
};

export const defaultInputObjectType = {
  name: '',
  kind: 'input_object',
  fields: [{ ...defaultField }],
};

export const defaultEnumValue = {
  value: '',
  description: '',
};

export const defaultEnumType = {
  name: '',
  kind: 'enum',
  values: [{ ...defaultEnumValue }],
};

export const gqlInbuiltTypes = [
  {
    name: 'Int',
    isInbuilt: true,
  },
  {
    name: 'String',
    isInbuilt: true,
  },
  {
    name: 'Float',
    isInbuilt: true,
  },
  {
    name: 'Boolean',
    isInbuilt: true,
  },
];
