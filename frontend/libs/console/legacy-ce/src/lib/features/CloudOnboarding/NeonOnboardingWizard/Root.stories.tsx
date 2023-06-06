import { ComponentMeta, Story } from '@storybook/react';
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
} as ComponentMeta<typeof Root>;

export const WithSurvey: Story = () => {
  const queryClient = useQueryClient();
  // need to invalidate as useSurveysData hook is using a stale time
  queryClient.invalidateQueries(SurveyQueryKey.fetchAllSurveyData, {
    refetchActive: false,
  });

  return <Root />;
};

WithSurvey.parameters = {
  msw: [
    ...mutationBaseHandlers(),
    onboardingDataEmptyActivity,
    fetchUnansweredSurveysHandler,
  ],
  chromatic: { disableSnapshot: true },
};

export const WithoutSurvey: Story = () => {
  const queryClient = useQueryClient();
  // need to invalidate as `useSurveysData` hook is using a stale time
  queryClient.invalidateQueries(SurveyQueryKey.fetchAllSurveyData, {
    refetchActive: false,
  });

  return <Root />;
};

WithoutSurvey.parameters = {
  msw: [
    ...mutationBaseHandlers(),
    onboardingDataEmptyActivity,
    fetchAnsweredSurveysHandler,
  ],
  chromatic: { disableSnapshot: true },
};
