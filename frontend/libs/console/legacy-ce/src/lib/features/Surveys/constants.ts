import { convertDateTimeToUTC } from './utils';

export const surveysQueryKey = 'surveysData';
export const surveyName = 'Hasura familiarity survey';

/**
 * GraphQl query to fetch all surveys related data
 */
export const fetchAllSurveysDataQuery = `
 query fetchAllSurveysData($currentTime: timestamptz!) {
   survey(where: {
       _or: [
         {ended_at: {_gte: $currentTime}}
         {ended_at: {_is_null: true}},
       ]
     }) {
     survey_name
     survey_questions {
       kind
       question
       id
       survey_question_options {
         option
         id
       }
     }
   }
   survey_question_answers {
    survey_question_id
   }
 }
  `;

export const fetchAllSurveysDataQueryVariables = {
  currentTime: convertDateTimeToUTC(new Date()),
};

/**
 * GraphQl mutation to save the survey answer
 */
export const addSurveyAnswerMutation = `
 mutation addSurveyAnswer ($responses: [QuestionAnswers]!, $surveyName: String!) {
    saveSurveyAnswer(payload: {responses: $responses, surveyName: $surveyName}) {
      status
    }
  }
  `;
