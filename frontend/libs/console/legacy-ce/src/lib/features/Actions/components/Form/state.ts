import { defaultHeader } from '@hasura/shared/types';
import {
  requestBodyActionState,
  requestTransformState,
  responseBodyActionState,
  responseTransformState,
} from '../../../ConfigureTransformation/requestTransformState';
import {
  RequestTransformState,
  ResponseTransformState,
} from '../../../ConfigureTransformation/stateDefaults';
import {
  getEnvVarsFromLS,
  getSessionVarsFromLS,
} from '../../../ConfigureTransformation/utils';
import { ActionState } from '../../types';
import { getActionRequestSampleInput } from './utils';

export const defaultRelFieldMapping = {
  column: '',
  field: '',
};

export const defaultActionDefSdl = `## Use "type Query" for query type actions
## Use "type Mutation" for mutation type actions

#type Query {
type Mutation {
  # Define your action here
  actionName (arg1: SampleInput!): SampleOutput
}
`;

export const defaultTypesDefSdl = `type SampleOutput {
  accessToken: String!
}

input SampleInput {
  username: String!
  password: String!
}
`;

export const defaultActionRequestBody = `{
  "users": {
    "name": {{$body.input.arg1.username}},
    "password": {{$body.input.arg1.password}}
  }
}`;

export const getActionRequestTransformDefaultState =
  (): RequestTransformState => {
    return {
      ...requestTransformState,
      envVars: getEnvVarsFromLS(),
      sessionVars: getSessionVarsFromLS(),
      requestQueryParams: [{ name: '', value: '' }],
      requestAddHeaders: [{ name: '', value: '' }],
      requestBody: {
        action: requestBodyActionState.transformApplicationJson,
        template: defaultActionRequestBody,
        form_template: [{ name: 'name', value: '{{$body.action.name}}' }],
      },
      requestSampleInput: JSON.stringify(
        getActionRequestSampleInput(defaultActionDefSdl, defaultTypesDefSdl),
      ),
    };
  };

export const getActionResponseTransformDefaultState =
  (): ResponseTransformState => {
    return {
      ...responseTransformState,
      responseBody: {
        action: responseBodyActionState.transformApplicationJson,
        template: defaultActionResponseBody,
        form_template: [{ name: 'name', value: '{{$body.action.name}}' }],
      },
    };
  };

export const defaultActionResponseBody = `{
  "response": {{$body}}
}`;

const getDefaultState = (
  defaultActionSdl?: string | null,
  defaultTypesSdl?: string | null,
): ActionState => ({
  handler: '',
  actionDefinition: {
    sdl: defaultActionSdl || defaultActionDefSdl,
    error: null,
    timer: null,
    ast: null,
  },
  typeDefinition: {
    sdl: defaultTypesSdl || defaultTypesDefSdl,
    error: null,
    timer: null,
    ast: null,
  },
  headers: [{ ...defaultHeader }],
  forwardClientHeaders: false,
  kind: 'synchronous',
  timeout: '',
  comment: '',
});

export default getDefaultState;
