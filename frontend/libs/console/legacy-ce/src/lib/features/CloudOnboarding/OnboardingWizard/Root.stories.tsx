import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';
import { useQueryClient } from 'react-query';
import { SurveyQueryKey } from '../../../features/Surveys';
import { Root } from './Root';
import {
  mutationBaseHandlers,
  fetchAnsweredSurveysHandler,
  fetchUnansweredSurveysHandler,
  onboardingDataEmptyActivity,
} from './mocks/handlers.mock';

export default {
  title: 'features/CloudOnboarding/Onboarding Wizard/Root',
  component: Root,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof Root>;

export const WithSurvey: StoryObj = {
  render: () => {
    const queryClient = useQueryClient();
    // need to invalidate as useSurveysData hook is using a stale time
    void queryClient.invalidateQueries(SurveyQueryKey.fetchAllSurveyData, {
      refetchActive: false,
    });

    return <Root />;
  },

  parameters: {
    msw: [
      ...mutationBaseHandlers(),
      onboardingDataEmptyActivity,
      fetchUnansweredSurveysHandler,
    ],
    chromatic: { disableSnapshot: true },
  },
};

export const WithoutSurvey: StoryObj = {
  render: () => {
    const queryClient = useQueryClient();
    // need to invalidate as `useSurveysData` hook is using a stale time
    void queryClient.invalidateQueries(SurveyQueryKey.fetchAllSurveyData, {
      refetchActive: false,
    });

    return <Root />;
  },

  parameters: {
    msw: [
      ...mutationBaseHandlers(),
      onboardingDataEmptyActivity,
      fetchAnsweredSurveysHandler,
    ],
    chromatic: { disableSnapshot: true },
  },
};
