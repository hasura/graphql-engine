import {
  defaultActionDefSdl,
  defaultTypesDefSdl,
  defaultHeader,
} from '../Common/stateDefaults';

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
