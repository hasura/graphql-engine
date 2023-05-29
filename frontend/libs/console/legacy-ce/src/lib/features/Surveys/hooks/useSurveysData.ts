import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from 'react-query';
import { APIError } from '../../../hooks/error';
import { SurveyResponseV2 } from '../../ControlPlane';
import {
  AllowedSurveyNames,
  SurveyData,
  SurveyQueryKey,
  SurveysResponseData,
} from '../types';
import {
  addSurveyAnswerMutationFn,
  fetchAllSurveysDataQueryFn,
} from '../utils';
import globals from '../../../Globals';

type UseSurveysDataResponse = {
  // Boolean variable which decides whether to show/hide survey.
  show: boolean;
  // A function to toggle show to true, from the calling component
  // based on some user action, condition etc.
  toggleSurvey: (show?: boolean) => void;
  onSkip: () => void;
  onSubmit: (arg: SurveyResponseV2[]) => void;
  data: SurveyData | undefined;
};

type UseSurveysDataProps = {
  surveyName: AllowedSurveyNames;
};

export function useSurveysData(
  props: UseSurveysDataProps
): UseSurveysDataResponse {
  const { surveyName } = props;
  const [show, setShow] = useState(true);

  const { isLoading, isError, data } = useQuery<SurveysResponseData, APIError>(
    SurveyQueryKey.fetchAllSurveyData,
    fetchAllSurveysDataQueryFn,
    {
      refetchOnWindowFocus: false,
    }
  );

  const queryClient = useQueryClient();
  const mutation = useMutation(addSurveyAnswerMutationFn, {
    onSuccess: () => {
      queryClient.refetchQueries(SurveyQueryKey.fetchAllSurveyData, {
        active: true,
      });
    },
  });

  const toggleSurvey = (show?: boolean) => {
    if (show) {
      setShow(show);
    } else {
      setShow(s => !s);
    }
  };

  const onSkip = () => {
    setShow(false);
  };

  const onSubmit = (responses: SurveyResponseV2[]) => {
    mutation.mutate({
      surveyName,
      projectID: globals.hasuraCloudProjectId,
      responses,
    });
    setShow(false);
  };

  const emptySurveyResponseData = {
    show: false,
    toggleSurvey,
    onSkip,
    onSubmit,
    data: undefined,
  };

  if (isLoading || isError) {
    return emptySurveyResponseData;
  }

  const surveyData = data?.data?.survey_v2?.find(
    surveyInfo => surveyInfo.survey_name === surveyName
  );

  if (!surveyData) {
    return emptySurveyResponseData;
  }

  let isSurveyAnswered = false;

  if (
    surveyData?.survey_responses &&
    typeof surveyData?.survey_responses !== 'undefined' &&
    surveyData?.survey_responses.length > 0
  ) {
    isSurveyAnswered = true;
  }

  // skip showing the survey form if survey is already answered
  // this logic can be moved inside of specific themes in future
  if (isSurveyAnswered) {
    return emptySurveyResponseData;
  }

  return {
    show,
    toggleSurvey,
    onSkip,
    onSubmit,
    data: surveyData,
  };
}
