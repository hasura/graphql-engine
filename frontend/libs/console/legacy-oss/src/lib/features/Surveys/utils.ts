import moment from 'moment';
import { cloudDataServiceApiClient } from '@/hooks/cloudDataServiceApiClient';
import { reactQueryClient } from '@/lib/reactQuery';
import {
  addSurveyAnswerMutation,
  fetchAllSurveysDataQuery,
  fetchAllSurveysDataQueryVariables,
  surveysQueryKey,
} from './constants';
import { QuestionAnswers, SurveysResponseData } from './types';

// cloud uses cookie-based auth, so does not require an admin secret
const headers = {
  'content-type': 'application/json',
};

export const fetchAllSurveysDataQueryFn = () =>
  cloudDataServiceApiClient<SurveysResponseData, SurveysResponseData>(
    fetchAllSurveysDataQuery,
    fetchAllSurveysDataQueryVariables,
    headers
  );

export const addSurveyAnswerMutationFn = (mutationVariables: QuestionAnswers) =>
  cloudDataServiceApiClient<SurveysResponseData, SurveysResponseData>(
    addSurveyAnswerMutation,
    mutationVariables,
    headers
  );

export const convertDateTimeToUTC = (dateTime: string | Date | number) => {
  return moment
    .utc(dateTime, moment.ISO_8601)
    .format('yyyy-MM-DDTHH:mm:ss.SSSSSZ');
};

export const prefetchSurveysData = () => {
  reactQueryClient.prefetchQuery(surveysQueryKey, fetchAllSurveysDataQueryFn);
};
