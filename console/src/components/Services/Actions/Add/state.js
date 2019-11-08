const defaultActionDefSdl = `
type ActionDef {
  userSignup (login: LoginInfo!): UserInfo
}`;

const defaultTypesDefSdl = `
type UserInfo {
  accessToken: String!
}

input LoginInfo {
  email: String!
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
