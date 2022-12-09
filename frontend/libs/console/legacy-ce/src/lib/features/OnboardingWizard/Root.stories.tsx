import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { useQueryClient } from 'react-query';
import { RootWithoutCloudCheck } from './Root';
import {
  mutationBaseHandlers,
  fetchAnsweredSurveysHandler,
  fetchUnansweredSurveysHandler,
  onboardingDataEmptyActivity,
} from './mocks/handlers.mock';
import { surveysQueryKey } from '../Surveys/constants';

export default {
  title: 'features/Onboarding Wizard/Root',
  component: RootWithoutCloudCheck,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof RootWithoutCloudCheck>;

export const WithSurvey: Story = () => {
  const queryClient = useQueryClient();
  // need to invalidate as useSurveysData hook is using a stale time
  queryClient.invalidateQueries(surveysQueryKey, {
    refetchActive: false,
  });

  return <RootWithoutCloudCheck />;
};

WithSurvey.parameters = {
  msw: [
    ...mutationBaseHandlers(),
    onboardingDataEmptyActivity,
    fetchUnansweredSurveysHandler,
  ],
};

export const WithoutSurvey: Story = () => {
  const queryClient = useQueryClient();
  // need to invalidate as `useSurveysData` hook is using a stale time
  queryClient.invalidateQueries(surveysQueryKey, {
    refetchActive: false,
  });

  return <RootWithoutCloudCheck />;
};

WithoutSurvey.parameters = {
  msw: [
    ...mutationBaseHandlers(),
    onboardingDataEmptyActivity,
    fetchAnsweredSurveysHandler,
  ],
};
