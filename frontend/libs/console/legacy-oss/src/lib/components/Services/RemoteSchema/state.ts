import { AsyncState, ListState, AddState } from './types';

const asyncState: AsyncState = {
  isRequesting: false,
  isError: false,
  isFetching: false,
  isFetchError: null,
};

const listState: ListState = {
  remoteSchemas: [],
  filtered: [],
  searchQuery: '',
  viewRemoteSchema: '',
  ...asyncState,
};

const addState: AddState = {
  manualUrl: '',
  envName: null,
  headers: [],
  timeoutConf: '',
  name: '',
  forwardClientHeaders: false,
  comment: '',
  customization: undefined,
  ...asyncState,
  editState: {
    id: -1,
    isModify: true,
    originalName: '',
    originalHeaders: [],
    originalUrl: '',
    originalEnvUrl: '',
    originalTimeoutConf: '',
    originalForwardClientHeaders: false,
    originalComment: '',
    originalCustomization: undefined,
  },
};

export { listState, addState };
