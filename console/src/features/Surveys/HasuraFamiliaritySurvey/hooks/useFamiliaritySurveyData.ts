import { useState } from 'react';
import { useMutation, useQuery, useQueryClient } from 'react-query';
import { APIError } from '@/hooks/error';
import { IconCardGroupItem } from '@/new-components/IconCardGroup';
import { SurveysResponseData } from '../../types';
import { mapOptionLabelToDetails } from './utils';
import { surveyName, surveysQueryKey } from '../../constants';
import {
  addSurveyAnswerMutationFn,
  fetchAllSurveysDataQueryFn,
} from '../../utils';

const emptySurveyResponseData = {
  showFamiliaritySurvey: false,
  data: { question: '', options: [] },
  onSkip: () => {},
  onOptionClick: () => {},
};

export function useFamiliaritySurveyData(): {
  showFamiliaritySurvey: boolean;
  data: { question: string; options: IconCardGroupItem<string>[] };
  onSkip: () => void;
  onOptionClick: (optionValue: string) => void;
} {
  const [showFamiliaritySurvey, setShowFamiliaritySurvey] = useState(true);

  const { isLoading, isError, data } = useQuery<SurveysResponseData, APIError>(
    surveysQueryKey,
    fetchAllSurveysDataQueryFn,
    {
      staleTime: 5 * 60 * 1000,
    }
  );

  const queryClient = useQueryClient();
  const mutation = useMutation(addSurveyAnswerMutationFn, {
    onSuccess: () => {
      queryClient.refetchQueries(surveysQueryKey, { active: true });
    },
  });

  if (isLoading || isError) {
    return emptySurveyResponseData;
  }

  const surveyQuestionData = data?.data?.survey?.find(
    surveyInfo => surveyInfo.survey_name === surveyName
  )?.survey_questions?.[0];

  // skip showing the survey form if survey has no questions
  if (!surveyQuestionData) {
    return emptySurveyResponseData;
  }

  const isSurveyAnswered = data?.data?.survey_question_answers?.some(
    answer => answer.survey_question_id === surveyQuestionData.id
  );

  // skip showing the survey form if survey is already answered
  if (isSurveyAnswered) {
    return emptySurveyResponseData;
  }

  const onSkip = () => {
    setShowFamiliaritySurvey(false);
    mutation.mutate({
      surveyName,
      responses: [
        {
          answer: 'Skipped hasura familiarity survey',
          question_id: surveyQuestionData.id,
        },
      ],
    });
  };

  const onOptionClick = (optionValue: string) => {
    setShowFamiliaritySurvey(false);
    mutation.mutate({
      surveyName,
      responses: [
        {
          optionsSelected: optionValue,
          question_id: surveyQuestionData.id,
        },
      ],
    });
  };

  return {
    showFamiliaritySurvey,
    data: {
      question: surveyQuestionData.question,
      options: mapOptionLabelToDetails(surveyQuestionData),
    },
    onSkip,
    onOptionClick,
  };
}
