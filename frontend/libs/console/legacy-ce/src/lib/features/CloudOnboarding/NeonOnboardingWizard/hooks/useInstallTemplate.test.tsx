import React, { ReactNode } from 'react';
import { waitFor, screen, render } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { setupServer } from 'msw/node';
import { Provider as ReduxProvider } from 'react-redux';
import { configureStore } from '@reduxjs/toolkit';
import {
  mockGithubServerDownHandler,
  fetchGithubMetadataHandler,
  fetchGithubMigrationHandler,
  metadataFailureHandler,
  metadataSuccessHandler,
  queryFailureHandler,
  querySuccessHandler,
} from '../mocks/handlers.mock';
import {
  mockMetadataUrl,
  mockMigrationUrl,
  MOCK_INITIAL_METADATA,
  serverDownErrorMessage,
} from '../mocks/constants';
import { useInstallTemplate } from './useInstallTemplate';
import { NEON_TEMPLATE_BASE_PATH } from '../../constants';
import 'whatwg-fetch';

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
  const { install } = useInstallTemplate(
    'default',
    NEON_TEMPLATE_BASE_PATH,
    onSuccessCb,
    onErrorCb
  );

  React.useEffect(() => {
    if (install) {
      install();
    }
  }, [install]);

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
    server.use(
      fetchGithubMetadataHandler,
      fetchGithubMigrationHandler,
      querySuccessHandler,
      metadataSuccessHandler
    );

    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect success callback to be called
    await waitFor(() => expect(onSuccessCb).toHaveBeenCalled());
  });

  it('fails to fetch metadata file from github, should call the error callback', async () => {
    server.use(
      mockGithubServerDownHandler(mockMetadataUrl),
      fetchGithubMigrationHandler,
      metadataSuccessHandler,
      querySuccessHandler
    );
    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after fetching metadata file from github fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalled());

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = `Failed to fetch metadata from the provided Url: ${mockMetadataUrl}`;
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });

  it('fails to fetch migration file from github, should call the error callback', async () => {
    server.use(
      mockGithubServerDownHandler(mockMigrationUrl),
      fetchGithubMetadataHandler,
      metadataSuccessHandler,
      querySuccessHandler
    );
    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after fetching migration file from github fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalled());

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = `Failed to fetch migration data from the provided Url: ${mockMigrationUrl}`;
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });

  it('fails to apply metadata to server, should call the error callback', async () => {
    server.use(
      fetchGithubMetadataHandler,
      fetchGithubMigrationHandler,
      querySuccessHandler,
      metadataFailureHandler
    );

    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after applying metadata to server fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalled());

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = JSON.stringify(serverDownErrorMessage);
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });

  it('fails to apply migration to server, should call the error callback', async () => {
    server.use(
      fetchGithubMetadataHandler,
      fetchGithubMigrationHandler,
      metadataSuccessHandler,
      queryFailureHandler
    );

    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after applying migration to server fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalled());

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = JSON.stringify(serverDownErrorMessage);
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });
});
