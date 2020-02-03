import {
  defaultActionDefSdl,
  defaultTypesDefSdl,
  defaultHeader,
} from '../Common/stateDefaults';

let defaultHandler = '';
if (typeof navigator !== 'undefined') {
  const isLinux = navigator.appVersion.toLowerCase().includes('linux');
  if (isLinux) {
    defaultHandler = 'http://localhost:3000';
  } else {
    defaultHandler = 'http://host.docker.internal';
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
};

export default state;
