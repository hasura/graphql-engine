import React from 'react';
import { render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { setupServer } from 'msw/node';
import { Provider as ReduxProvider } from 'react-redux';
import { configureStore } from '@reduxjs/toolkit';
import { GrowthExperimentsClient } from '../GrowthExperiments';
import { baseHandlers } from './mocks/handlers.mock';
import { mockGrowthClient } from './mocks/constants';
import { RootWithoutCloudCheck } from './Root';

const server = setupServer(...baseHandlers());

beforeAll(() => server.listen());
afterAll(() => server.close());

const OnboardingWizardRender = async (
  mockedGrowthClient: GrowthExperimentsClient
) => {
  // redux provider is needed as ConnectDBScreen is using dispatch to push routes
  const store = configureStore({
    reducer: {
      tables: () => ({ currentDataSource: 'postgres' }),
    },
  });

  // react query provider is needed as surveys component is using it
  const reactQueryClient = new QueryClient();

  render(
    <ReduxProvider store={store} key="provider">
      <QueryClientProvider client={reactQueryClient}>
        <RootWithoutCloudCheck growthExperimentsClient={mockedGrowthClient} />
      </QueryClientProvider>
    </ReduxProvider>
  );
};

describe('Check different configurations of experiments client', () => {
  it('should hide wizard as error in experiment name', async () => {
    await OnboardingWizardRender(mockGrowthClient.nameError);

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).not.toBeInTheDocument();
  });

  it('should show wizard as experiment is enabled and user activity is empty', async () => {
    await OnboardingWizardRender(mockGrowthClient.enabledWithoutActivity);

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).toBeInTheDocument();
  });

  it('should hide wizard as experiment is disabled', async () => {
    await OnboardingWizardRender(mockGrowthClient.disabledWithoutActivity);

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).not.toBeInTheDocument();
  });

  it('should hide wizard as user has completed the onboarding', async () => {
    await OnboardingWizardRender(mockGrowthClient.enabledWithCorrectActivity);

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).not.toBeInTheDocument();
  });

  it('should hide wizard as experiment is disabled, user activity does not matter in this case', async () => {
    await OnboardingWizardRender(mockGrowthClient.disabledWithCorrectActivity);

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).not.toBeInTheDocument();
  });

  it('should show wizard as experiment is enabled, and user activity is incorrect. Could mean a error on backend.', async () => {
    await OnboardingWizardRender(mockGrowthClient.enabledWithWrongActivity);

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).toBeInTheDocument();
  });
});
