import { AllowedRestMethods, RestEndpoint } from '@hasura/shared/types';

export const REST_API_LIST_PATH = '/api/rest/list';

export type RestEndpointCreateAction = (
  restEndpoint: RestEndpoint,
  request: string,
  cb: () => void,
) => Promise<unknown>;

export type RestEndpointEditAction = (
  restEndpoint: RestEndpoint,
  request: string,
  cb: () => void,
  currentRestEndpoint: RestEndpoint,
) => Promise<unknown>;

export type RestEndpointFormStateHook = {
  formState: RestEndpointFormState;
  formSubmitHandler: RestEndpointFormSubmitHandler;
};

export type RestEndpointFormState = {
  name?: string;
  comment?: string;
  url?: string;
  methods?: AllowedRestMethods[];
  request?: string;
};

export type RestEndpointFormSubmitHandler = (
  restEndpoint: RestEndpoint,
  request: string,
  cb: () => void,
  currentRestEndpoint?: RestEndpoint,
) => Promise<unknown>;
