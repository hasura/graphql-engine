import {
  defaultArg,
  gqlInbuiltTypes,
  defaultScalarType,
} from '../Common/stateDefaults';

const state = {
  name: '',
  webhook: '',
  types: [
    ...JSON.parse(JSON.stringify(gqlInbuiltTypes)),
    { ...defaultScalarType },
  ],
  arguments: [{ ...defaultArg }],
  outputType: '',
};

export default state;
