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
  arguments: [{ ...defaultArg }],
};

export const defaultInputObjectType = {
  name: '',
  kind: 'input_object',
  fields: [{ ...defaultField }],
};
