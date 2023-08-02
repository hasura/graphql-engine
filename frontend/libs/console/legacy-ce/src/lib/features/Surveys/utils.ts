import moment from 'moment';
import { reactQueryClient } from '../../lib/reactQuery';
import {
  FETCH_ALL_SURVEYS_DATA,
  ADD_SURVEY_ANSWER,
  SuccessOrError,
  controlPlaneClient,
  SaveSurveyAnswerV2Payload,
} from '../ControlPlane';
import { SurveyData, SurveyQueryKey, SurveysResponseData } from './types';

export const convertDateTimeToUTC = (dateTime: string | Date | number) => {
  return moment
    .utc(dateTime, moment.ISO_8601)
    .format('yyyy-MM-DDTHH:mm:ss.SSSSSZ');
};

export const fetchAllSurveysDataQueryVariables = {
  currentTime: convertDateTimeToUTC(new Date()),
};

export const fetchAllSurveysDataQueryFn = () =>
  controlPlaneClient.query<SurveysResponseData>(
    FETCH_ALL_SURVEYS_DATA,
    fetchAllSurveysDataQueryVariables
  );

export const addSurveyAnswerMutationFn = (
  mutationVariables: SaveSurveyAnswerV2Payload
) =>
  controlPlaneClient.query<SuccessOrError, SaveSurveyAnswerV2Payload>(
    ADD_SURVEY_ANSWER,
    mutationVariables
  );

export const prefetchSurveysData = () => {
  reactQueryClient.prefetchQuery(
    SurveyQueryKey.fetchAllSurveyData,
    fetchAllSurveysDataQueryFn
  );
};

interface ArrayItem {
  position: number;
}

/**
 * This function returns -1, 0, or 1 randomly
 */
function getRandomInt() {
  return Math.floor(Math.random() * 3) - 1;
}

/**
 * This function orders the array items in the following order.
 * If the `position` key is different for two items `a` and `b`, we sort them in ascending order.
 * If the `position` key is same for two items `a` and `b`, we shuffle them in random order.
 */
export function orderArrayItems<T extends ArrayItem>(unroderedArray: T[]) {
  const orderedArray = unroderedArray.sort((a, b) => {
    if (a.position === b.position) {
      return getRandomInt();
    } else return a.position - b.position;
  });

  return orderedArray;
}

export function getButtonText(show: boolean, data?: SurveyData) {
  let submitText = 'Submit';
  let cancelText = 'Cancel';
  if (!show) return { submitText, cancelText };

  try {
    const templateConfig = JSON.parse(data?.template_config);
    if (templateConfig.survey_submit_text) {
      submitText = templateConfig.survey_submit_text;
    }
    if (templateConfig.survey_cancel_text) {
      cancelText = templateConfig.survey_cancel_text;
    }
  } catch (e) {
    // console.error(e);
  }

  return { submitText, cancelText };
}
