import { GraphQLError } from 'graphql';
import { FetchAllSurveysDataQuery, SurveyResponseV2 } from '../ControlPlane';

export type SurveysResponseData = {
  data?: FetchAllSurveysDataQuery;
  errors?: GraphQLError[];
};

export type SurveyData = FetchAllSurveysDataQuery['survey_v2'][0];

export type SurveyQuestionData = SurveyData['survey_questions'][0];

export type SurveyQuestionProps = {
  questionData: SurveyQuestionData;
  responses: SurveyResponseV2[];
  setResponses: React.Dispatch<React.SetStateAction<SurveyResponseV2[]>>;
};

export enum AllowedSurveyThemes {
  // Theme description: Theme which renders hasura familiarity survey
  familiaritySurveyTheme = 'Familiarity Survey Theme',
}

export enum AllowedSurveyNames {
  deleteSurveyName = 'Hasura Cloud Project Delete Survey',
  downgradeSurveyName = 'Hasura Cloud Project Downgrade Survey',
  familiaritySurvey = 'Hasura familiarity survey',
}

export enum SurveyQueryKey {
  fetchAllSurveyData = 'survey:fetchAllSurveyData',
}
