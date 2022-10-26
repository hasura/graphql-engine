import React, { ReactNode } from 'react';
import { waitFor, screen, render } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { setupServer } from 'msw/node';
import { Provider as ReduxProvider } from 'react-redux';
import { configureStore } from '@reduxjs/toolkit';
import {
  fetchGithubMigrationHandler,
  mockGithubServerDownHandler,
  querySuccessHandler,
  queryFailureHandler,
} from '../mocks/handlers.mock';
import {
  mockMigrationUrl,
  MOCK_INITIAL_METADATA,
  serverDownErrorMessage,
} from '../mocks/constants';
import { useInstallMigration } from './useInstallMigration';

const server = setupServer();

let reactQueryClient = new QueryClient();

beforeAll(() => server.listen({ onUnhandledRequest: 'warn' }));
beforeEach(() => {
  // provide a fresh reactQueryClient for each test to prevent state caching among tests
  reactQueryClient = new QueryClient();

  // don't retry failed queries, overrides the default behaviour. This is done as otherwise we'll
  // need to add a significant wait time (~10000 ms) to the test to wait for all the 3 retries (react-query default)
  // to fail, for the error callback to be called. Till then the state is loading.
  reactQueryClient.setDefaultOptions({
    queries: {
      retry: false,
    },
  });
});
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

const onSuccessCb = jest.fn(() => {});

const onErrorCb = jest.fn(() => {});

const Component = () => {
  // fetch the function to apply migration
  const { performMigration } = useInstallMigration(
    'default',
    mockMigrationUrl,
    onSuccessCb,
    onErrorCb
  );

  React.useEffect(() => {
    if (performMigration) {
      performMigration();
    }
  }, [performMigration]);

  return <div>Welcome</div>;
};

type Props = {
  children?: ReactNode;
};

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

describe('Check useInstallMigration installs the correct migrations', () => {
  it('should install the correct migration and call success callback', async () => {
    server.use(fetchGithubMigrationHandler, querySuccessHandler);

    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect success callback to be called
    await waitFor(() => expect(onSuccessCb).toHaveBeenCalledTimes(1));
  });

  it('fails to fetch migration file from github, should call the error callback', async () => {
    server.use(
      mockGithubServerDownHandler(mockMigrationUrl),
      querySuccessHandler
    );
    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after fetching migration file from github fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledTimes(1));

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = `Failed to fetch migration data from the provided Url: ${mockMigrationUrl}`;
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });

  it('fails to apply migration to server, should call the error callback', async () => {
    server.use(fetchGithubMigrationHandler, queryFailureHandler);

    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after applying migration to server fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledTimes(1));

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = JSON.stringify(serverDownErrorMessage);
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });
});
