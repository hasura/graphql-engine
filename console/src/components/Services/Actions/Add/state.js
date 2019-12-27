const defaultActionDefSdl = `
type Mutation {
  ## Define your action as a mutation here
  actionName (arg1: SampleInput!): SampleOutput
}`;

const defaultTypesDefSdl = `
type SampleOutput {
  accessToken: String!
}

input SampleInput {
  username: String!
  password: String!
}

`;

const state = {
  handler: '',
  actionDefinition: {
    sdl: defaultActionDefSdl,
    error: '',
    timer: null,
    ast: null,
  },
  typeDefinition: {
    sdl: defaultTypesDefSdl,
    error: '',
    timer: null,
    ast: null,
  },
  kind: 'synchronous',
  isFetching: false,
};

export default state;
