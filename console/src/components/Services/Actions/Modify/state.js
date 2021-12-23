import { defaultHeader } from '../Common/stateDefaults';

const state = {
  handler: '',
  kind: 'synchronous',
  actionDefinition: {
    sdl: '',
    error: null,
  },
  typeDefinition: {
    sdl: '',
    error: null,
  },
  isFetching: false,
  headers: [defaultHeader],
  forwardClientHeaders: false,
  timeout: '',
  comment: '',
};

export default state;
