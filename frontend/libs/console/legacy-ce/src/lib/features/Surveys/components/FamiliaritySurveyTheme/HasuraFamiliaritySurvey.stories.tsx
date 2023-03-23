import { ComponentMeta, Story } from '@storybook/react';
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
} as ComponentMeta<typeof FamiliaritySurveyTheme>;

export const Base: Story = () => (
  <FamiliaritySurveyTheme
    orderedSurveyQuestionData={
      mockFetchAllSurveysData?.data?.survey_v2?.[2].survey_questions ?? []
    }
    handleSubmit={() => {}}
  />
);

Base.parameters = {
  chromatic: { disableSnapshot: true },
};
