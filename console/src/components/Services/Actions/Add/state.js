export const defaultArg = {
  name: '',
  type: '',
};

const state = {
  name: '',
  webhook: '',
  types: [
    {
      name: '',
      kind: '',
      description: '',
      optional: false,
    },
  ],
  arguments: [{ ...defaultArg }],
  outputType: '',
};

export default state;
