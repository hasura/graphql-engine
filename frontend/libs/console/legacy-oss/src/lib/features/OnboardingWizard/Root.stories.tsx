import React from 'react';
import { ComponentMeta, Story } from '@storybook/react';
import { ReactQueryDecorator } from '@/storybook/decorators/react-query';
import { useQueryClient } from 'react-query';
import { Root } from './Root';
import {
  baseHandlers,
  fetchAnsweredSurveysHandler,
  fetchUnansweredSurveysHandler,
} from './mocks/handlers.mock';
import { mockGrowthClient } from './mocks/constants';
import { surveysQueryKey } from '../Surveys/constants';

export default {
  title: 'features/Onboarding Wizard/Root',
  component: Root,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof Root>;

export const WithSurvey: Story = () => {
  const queryClient = useQueryClient();
  // need to invalidate as useSurveysData hook is using a stale time
  queryClient.invalidateQueries(surveysQueryKey, {
    refetchActive: false,
  });

  return (
    <Root growthExperimentsClient={mockGrowthClient.enabledWithoutActivity} />
  );
};

WithSurvey.parameters = {
  msw: [...baseHandlers(), fetchUnansweredSurveysHandler],
};

export const WithoutSurvey: Story = () => {
  const queryClient = useQueryClient();
  // need to invalidate as `useSurveysData` hook is using a stale time
  queryClient.invalidateQueries(surveysQueryKey, {
    refetchActive: false,
  });

  return (
    <Root growthExperimentsClient={mockGrowthClient.enabledWithoutActivity} />
  );
};

WithoutSurvey.parameters = {
  msw: [...baseHandlers(), fetchAnsweredSurveysHandler],
};
