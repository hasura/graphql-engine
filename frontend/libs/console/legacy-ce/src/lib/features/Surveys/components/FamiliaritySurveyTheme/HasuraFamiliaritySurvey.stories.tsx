import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { FamiliaritySurveyTheme } from './FamiliaritySurveyTheme';
import { mockFetchAllSurveysData } from '../../__mocks__/surveys.mock';

export default {
  title: 'features/Surveys/HasuraFamiliaritySurvey',
  component: FamiliaritySurveyTheme,
  decorators: [ReactQueryDecorator()],
  // disable chromatic snapshots as chromatic will show diff on each build
  // as the questions and options are randomly arranged on each render.
  chromatic: { disableSnapshot: true },
} as Meta<typeof FamiliaritySurveyTheme>;

export const Base: StoryObj = {
  render: () => (
    <FamiliaritySurveyTheme
      orderedSurveyQuestionData={
        mockFetchAllSurveysData?.data?.survey_v2?.[2].survey_questions ?? []
      }
      handleSubmit={() => {}}
    />
  ),

  parameters: {
    chromatic: { disableSnapshot: true },
  },
};
