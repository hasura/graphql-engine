import React, { ReactNode } from 'react';
import { render, screen } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { setupServer } from 'msw/node';
import { Provider as ReduxProvider } from 'react-redux';
import { configureStore } from '@reduxjs/toolkit';
import {
  baseHandlers,
  fetchGithubMigrationHandler,
  fetchGithubMetadataHandler,
  fetchUnansweredSurveysHandler,
  metadataSuccessHandler,
  querySuccessHandler,
  mockGithubServerDownHandler,
} from './mocks/handlers.mock';
import {
  mockGrowthClient,
  mockSampleQueryUrl,
  mockSchemaImageUrl,
  MOCK_INITIAL_METADATA,
} from './mocks/constants';
import { RootWithoutCloudCheck } from './Root';

const server = setupServer(
  ...baseHandlers(),
  fetchUnansweredSurveysHandler,
  mockGithubServerDownHandler(mockSampleQueryUrl),
  mockGithubServerDownHandler(mockSchemaImageUrl),
  fetchGithubMigrationHandler,
  fetchGithubMetadataHandler,
  metadataSuccessHandler,
  querySuccessHandler
);

// react query provider is needed as surveys component is using it
const reactQueryClient = new QueryClient();

beforeAll(() => {
  server.listen();
  // don't retry failed queries, overrides the default behaviour. This is done as otherwise we'll
  // need to add a significant wait time (~10000 ms) to the test to wait for all the 3 retries (react-query default)
  // to fail, for the error callback to be called. Till then the state is loading.
  reactQueryClient.setDefaultOptions({
    queries: {
      retry: false,
    },
  });
});
afterAll(() => server.close());

type Props = {
  children?: ReactNode;
};

// redux provider is needed as ConnectDBScreen is using dispatch to push routes
const store = configureStore({
  reducer: {
    tables: () => ({ currentDataSource: 'postgres', dataHeaders: {} }),
    metadata: () => ({
      metadataObject: MOCK_INITIAL_METADATA,
    }),
  },
});

const wrapper = ({ children }: Props) => (
  <ReduxProvider store={store} key="provider">
    <QueryClientProvider client={reactQueryClient}>
      {children}
    </QueryClientProvider>
  </ReduxProvider>
);

describe('Check different configurations of experiments client', () => {
  it('should hide wizard as error in experiment name', () => {
    render(
      <RootWithoutCloudCheck
        growthExperimentsClient={mockGrowthClient.nameError}
      />,
      { wrapper }
    );

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).not.toBeInTheDocument();
  });

  it('should show wizard as experiment is enabled and user activity is empty', () => {
    render(
      <RootWithoutCloudCheck
        growthExperimentsClient={mockGrowthClient.enabledWithoutActivity}
      />,
      { wrapper }
    );

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).toBeInTheDocument();
  });

  it('should hide wizard as experiment is disabled', () => {
    render(
      <RootWithoutCloudCheck
        growthExperimentsClient={mockGrowthClient.disabledWithoutActivity}
      />,
      { wrapper }
    );

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).not.toBeInTheDocument();
  });

  it('should hide wizard as user has completed the onboarding', () => {
    render(
      <RootWithoutCloudCheck
        growthExperimentsClient={mockGrowthClient.enabledWithCorrectActivity}
      />,
      { wrapper }
    );

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).not.toBeInTheDocument();
  });

  it('should hide wizard as experiment is disabled, user activity does not matter in this case', () => {
    render(
      <RootWithoutCloudCheck
        growthExperimentsClient={mockGrowthClient.disabledWithCorrectActivity}
      />,
      { wrapper }
    );

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).not.toBeInTheDocument();
  });

  it('should show wizard as experiment is enabled, and user activity is incorrect. Could mean a error on backend.', () => {
    render(
      <RootWithoutCloudCheck
        growthExperimentsClient={mockGrowthClient.enabledWithWrongActivity}
      />,
      { wrapper }
    );

    expect(
      screen.queryByText('Welcome to your new Hasura project!')
    ).toBeInTheDocument();
  });
});
