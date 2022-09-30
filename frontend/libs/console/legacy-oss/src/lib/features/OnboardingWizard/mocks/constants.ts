import { GrowthExperimentsClient } from '@/features/GrowthExperiments';
import { SurveysResponseData } from '@/features/Surveys';

export const mockGrowthClient: Record<string, GrowthExperimentsClient> = {
  /**
   * config to hide wizard as experiment name error
   */
  nameError: {
    getAllExperimentConfig: () => [
      {
        experiment: 'console_onboarding_wizardddd_v1',
        status: 'enabled',
        metadata: {},
        userActivity: {},
      },
    ],
    setAllExperimentConfig: () => Promise.resolve(),
  },
  /**
   *  config to show wizard as experiment enabled and
   *  user activity is empty
   */
  enabledWithoutActivity: {
    getAllExperimentConfig: () => [
      {
        experiment: 'console_onboarding_wizard_v1',
        status: 'enabled',
        metadata: {},
        userActivity: {},
      },
    ],
    setAllExperimentConfig: () => Promise.resolve(),
  },
  /**
   *  config to hide wizard as experiment is disabled
   */
  disabledWithoutActivity: {
    getAllExperimentConfig: () => [
      {
        experiment: 'console_onboarding_wizard_v1',
        status: 'disabled',
        metadata: {},
        userActivity: {},
      },
    ],
    setAllExperimentConfig: () => Promise.resolve(),
  },
  /**
   *  config to hide wizard as user has completed
   *  the onboarding
   */
  enabledWithCorrectActivity: {
    getAllExperimentConfig: () => [
      {
        experiment: 'console_onboarding_wizard_v1',
        status: 'enabled',
        metadata: {},
        userActivity: {
          onboarding_complete: true,
        },
      },
    ],
    setAllExperimentConfig: () => Promise.resolve(),
  },
  /**
   *  config to hide wizard as experiment is disabled,
   *  user has completed the onboarding or not should not matter
   *  in this case.
   */
  disabledWithCorrectActivity: {
    getAllExperimentConfig: () => [
      {
        experiment: 'console_onboarding_wizard_v1',
        status: 'disabled',
        metadata: {},
        userActivity: {
          onboarding_complete: true,
        },
      },
    ],
    setAllExperimentConfig: () => Promise.resolve(),
  },
  /**
   *  config to show wizard as experiment is enabled,
   *  and user activity is incorrect. Could mean a error on backend
   *  while saving user activity.
   */
  enabledWithWrongActivity: {
    getAllExperimentConfig: () => [
      {
        experiment: 'console_onboarding_wizard_v1',
        status: 'enabled',
        metadata: {},
        userActivity: {
          onboarding_completessss: true,
        },
      },
    ],
    setAllExperimentConfig: () => Promise.resolve(),
  },
};

export const fetchSurveysDataResponse: Record<
  string,
  SurveysResponseData['data']
> = {
  unanswered: {
    survey: [
      {
        survey_name: 'Hasura familiarity survey',
        survey_questions: [
          {
            kind: 'radio',
            question: 'How familiar are you with Hasura?',
            id: '4595916a-1c5b-4d55-b7ca-11616131d1d3',
            survey_question_options: [
              {
                option: 'new user',
                id: 'dcd2480b-cc1b-4e1a-b111-27aed8b89b8b',
              },
              {
                option: 'active user',
                id: '53ed19af-7ae7-4225-9969-13b06d9b8f66',
              },
              {
                option: 'recurring user',
                id: '8f81e3d0-f45b-40ba-9456-8865a5a1cb93',
              },
              {
                option: 'past user',
                id: 'b25edd78-0af3-4448-8302-14e2b818c4c6',
              },
            ],
          },
        ],
      },
    ],
    survey_question_answers: [],
  },
  answered: {
    survey: [
      {
        survey_name: 'Hasura familiarity survey',
        survey_questions: [
          {
            kind: 'radio',
            question: 'How familiar are you with Hasura?',
            id: '4595916a-1c5b-4d55-b7ca-11616131d1d3',
            survey_question_options: [
              {
                option: 'new user',
                id: 'dcd2480b-cc1b-4e1a-b111-27aed8b89b8b',
              },
              {
                option: 'active user',
                id: '53ed19af-7ae7-4225-9969-13b06d9b8f66',
              },
              {
                option: 'recurring user',
                id: '8f81e3d0-f45b-40ba-9456-8865a5a1cb93',
              },
              {
                option: 'past user',
                id: 'b25edd78-0af3-4448-8302-14e2b818c4c6',
              },
            ],
          },
        ],
      },
    ],
    survey_question_answers: [
      {
        survey_question_id: '4595916a-1c5b-4d55-b7ca-11616131d1d3',
      },
    ],
  },
};
