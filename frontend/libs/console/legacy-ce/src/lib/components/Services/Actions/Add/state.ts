import {
  defaultActionDefSdl,
  defaultTypesDefSdl,
  defaultHeader,
} from '../Common/stateDefaults';
import { DefaultState } from './types';

const state: DefaultState = {
  handler: '',
  actionDefinition: {
    sdl: defaultActionDefSdl,
    error: null,
    timer: null,
    ast: null,
  },
  typeDefinition: {
    sdl: defaultTypesDefSdl,
    error: null,
    timer: null,
    ast: null,
  },
  headers: [defaultHeader],
  forwardClientHeaders: false,
  kind: 'synchronous',
  isFetching: false,
  derive: {
    operation: '',
  },
  timeout: '',
  comment: '',
};

export default state;
