import { cloudDataServiceApiClient } from '../../hooks/cloudDataServiceApiClient';
import { trackOnboardingActivityMutation } from './constants';
import { clickRunQueryButton } from '../ApiExplorer/components/GraphiQLWrapper/utils';
import { programmaticallyTraceError } from '@hasura/shared/analytics';

export const runQueryInGraphiQL = () => {
  clickRunQueryButton();
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
    cloudHeaders,
  ).catch((error) => {
    programmaticallyTraceError(error);
  });
};
