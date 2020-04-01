export const defaultArg = {
  name: '',
  type: '',
  description: '',
  typeWrap: '0',
};

export const defaultField = {
  name: '',
  type: '',
  typeWrap: '0',
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

export const defaultActionDefSdl = `type Mutation {
  ## Define your action as a mutation here
  actionName (arg1: SampleInput!): SampleOutput
}
`;

export const defaultTypesDefSdl = `type SampleOutput {
  accessToken: String!
}

input SampleInput {
  username: String!
  password: String!
}
`;

export const defaultHeader = {
  name: '',
  value: '',
  type: 'static',
};

export const defaultRelFieldMapping = {
  column: '',
  field: '',
};

export const defaultRelConfig = {
  name: '',
  type: '',
  refSchema: '',
  refTable: '',
  fieldMapping: [defaultRelFieldMapping],
};
