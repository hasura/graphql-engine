import { AllowedRESTMethods } from '../../../../../metadata/types';

type CreateEndpointState = {
  name: string;
  comment: string;
  url: string;
  methods: AllowedRESTMethods[];
  request: string;
};

const defaultState: CreateEndpointState = {
  name: '',
  comment: '', // Description on the UI
  url: '', // Location on the UI
  methods: [],
  request: '',
  // errorHandlingMode: '' -> nothing on the API, skipping this for now
};

export { defaultState, CreateEndpointState };
