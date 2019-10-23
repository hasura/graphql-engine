export const defaultArg = {
  name: '',
  type: '',
  description: null,
};

const state = {
  name: '',
  webhook: '',
  types: {
    input_objects: [],
    objects: [],
    scalars: [],
    enums: [],
  },
  arguments: [{ ...defaultArg }],
  outputType: '',
};

export default state;
