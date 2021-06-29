import { AllowedRESTMethods } from '../../../../../metadata/types';

type CreateEndpointState = {
  name: string; // Name on UI
  comment: string; // Description on UI
  url: string; // Location on UI
  methods: AllowedRESTMethods[]; // Methods on the UI
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
