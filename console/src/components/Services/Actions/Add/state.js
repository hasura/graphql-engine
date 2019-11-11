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
  webhook: '',
  actionDefinition: {
    sdl: defaultActionDefSdl,
    error: '',
  },
  typeDefinition: {
    sdl: defaultTypesDefSdl,
    error: '',
  },
  kind: 'synchronous',
  isFetching: false,
};

export default state;
