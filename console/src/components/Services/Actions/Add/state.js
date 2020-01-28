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

let defaultHandler = '';
if (typeof navigator !== 'undefined') {
  const isLinux = navigator.appVersion.toLowerCase().includes('linux');
  if (isLinux) {
    defaultHandler = 'http://localhost:3000';
  } else {
    defaultHandler = 'http://host.docker.internal';
  }
}

const state = {
  handler: defaultHandler,
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
