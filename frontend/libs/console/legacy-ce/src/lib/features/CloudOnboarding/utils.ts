import {
  clickRunQueryButton,
  forceChangeGraphiqlQuery,
  forceGraphiQLIntrospection,
} from '../../components/Services/ApiExplorer/OneGraphExplorer/utils';
import { Dispatch } from '../../types';
import { cloudDataServiceApiClient } from '../../hooks/cloudDataServiceApiClient';
import { trackOnboardingActivityMutation } from './constants';
import { programmaticallyTraceError } from '../Analytics/core/programmaticallyTraceError';

export const runQueryInGraphiQL = () => {
  clickRunQueryButton();
};

export const fillSampleQueryInGraphiQL = (
  query: string,
  dispatch: Dispatch
) => {
  forceGraphiQLIntrospection(dispatch);

  // this timeout makes sure that there's a delay in setting query after introspection has been fired
  // this timeout does not intend to wait for introspection to finish
  setTimeout(() => {
    forceChangeGraphiqlQuery(query, dispatch);
  }, 500);
};

type ResponseDataOnMutation = {
  data: {
    trackOnboardingActivity: {
      status: string;
    };
  };
};

const cloudHeaders = {
  'content-type': 'application/json',
};

export const emitOnboardingEvent = (variables: Record<string, unknown>) => {
  // mutate server data
  cloudDataServiceApiClient<ResponseDataOnMutation, ResponseDataOnMutation>(
    trackOnboardingActivityMutation,
    variables,
    cloudHeaders
  ).catch(error => {
    programmaticallyTraceError(error);
  });
};
