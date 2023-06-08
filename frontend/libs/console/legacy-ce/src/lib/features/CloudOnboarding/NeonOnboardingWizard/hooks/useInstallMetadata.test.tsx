import React, { ReactNode } from 'react';
import { matchRequestUrl, MockedRequest } from 'msw';
import { waitFor, screen, render } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from 'react-query';
import { setupServer } from 'msw/node';
import { Provider as ReduxProvider } from 'react-redux';
import { configureStore } from '@reduxjs/toolkit';
import {
  fetchGithubMetadataHandler,
  metadataFailureHandler,
  metadataSuccessHandler,
  mockGithubServerDownHandler,
} from '../mocks/handlers.mock';
import { useInstallMetadata } from './useInstallMetadata';
import Endpoints from '../../../../Endpoints';
import {
  mockMetadataUrl,
  MOCK_INITIAL_METADATA,
  serverDownErrorMessage,
} from '../mocks/constants';
import 'whatwg-fetch';

const server = setupServer();

function waitForRequest(method: string, url: string) {
  let requestId = '';
  return new Promise<MockedRequest>((resolve, reject) => {
    server.events.on('request:start', async req => {
      const matchesMethod = req.method.toLowerCase() === method.toLowerCase();
      const matchesUrl = matchRequestUrl(req.url, url).matches;
      let matchesType = false;

      try {
        const reqbody = await req.json();
        matchesType = reqbody?.type === 'replace_metadata';
      } catch (err) {
        // not a metadata request, can be ignored
      }

      if (matchesMethod && matchesUrl && matchesType) {
        requestId = req.id;
      }
    });
    server.events.on('request:match', req => {
      if (req.id === requestId) {
        resolve(req);
      }
    });
    server.events.on('request:unhandled', req => {
      if (req.id === requestId) {
        reject(
          new Error(`The ${req.method} ${req.url.href} request was unhandled.`)
        );
      }
    });
  });
}

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
  const { updateMetadata } = useInstallMetadata(
    'default',
    mockMetadataUrl,
    onSuccessCb,
    onErrorCb
  );

  React.useEffect(() => {
    if (updateMetadata) {
      updateMetadata();
    }
  }, [updateMetadata]);

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

describe('Check useInstallMetadata installs the correct metadata', () => {
  it('should install the correct metadata and call success callback', async () => {
    server.use(fetchGithubMetadataHandler, metadataSuccessHandler);
    const pendingRequest = waitForRequest('POST', Endpoints.metadata);

    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect success callback to be called, after successful `replace_metadata` request
    await waitFor(() => expect(onSuccessCb).toHaveBeenCalledTimes(1));

    // STEP 3: expect the correct metadata being sent to the server
    const replaceMetadataRequest = await pendingRequest;
    expect(JSON.parse(replaceMetadataRequest.body as string)).toMatchSnapshot();
  });

  it('fails to fetch metadata file from github, should call the error callback', async () => {
    server.use(
      mockGithubServerDownHandler(mockMetadataUrl),
      metadataSuccessHandler
    );
    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after fetching metadata file from github fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledTimes(1));

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = `Failed to fetch metadata from the provided Url: ${mockMetadataUrl}`;
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });

  it('fails to apply metadata to server, should call the error callback', async () => {
    server.use(fetchGithubMetadataHandler, metadataFailureHandler);

    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.queryByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after applying metadata to server fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledTimes(1));

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = JSON.stringify(serverDownErrorMessage);
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });
});
