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
  timeout: 30,
  comment: '',
};

export default state;
