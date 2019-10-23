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
    },
  ],
  arguments: [{ ...defaultArg }],
  outputType: '',
};

export default state;
