const samplePayload = {
    "action_name": "actionName1",
    "sdl": {
      "complete": "type Mutation { actionName1 (arg1: SampleInput!): SampleOutput } type SampleOutput { accessToken: String! } input SampleInput { username: String! password: String! } type Mutation { actionName2 (arg1: SampleInput!): SampleOutput }"
    },
    "codegen_config": {
      "framework": "typescript-zeit"
    }
};

module.exports = {
  samplePayload
};
