import {
  defaultActionDefSdl,
  defaultTypesDefSdl,
  defaultHeader,
} from '../Common/stateDefaults';
import { DefaultState } from './types';

let defaultHandler = '';
if (typeof navigator !== 'undefined') {
  const { appVersion } = navigator;
  const isLinux =
    appVersion.toLowerCase().includes('linux') ||
    appVersion.toLowerCase().includes('x11');
  if (isLinux) {
    defaultHandler = 'http://localhost:3000';
  } else {
    defaultHandler = 'http://host.docker.internal:3000';
  }
}

const state: DefaultState = {
  handler: defaultHandler,
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
  execution: 'synchronous',
  isFetching: false,
  derive: {
    operation: '',
  },
  timeout: '',
  comment: '',
};

export default state;
