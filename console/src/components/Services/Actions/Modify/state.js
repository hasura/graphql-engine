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
  kind: 'synchronous',
  outputType: '',
  isFetching: false,
};

export default state;
