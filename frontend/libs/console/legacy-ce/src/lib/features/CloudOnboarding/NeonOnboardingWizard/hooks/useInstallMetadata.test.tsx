import React, { ReactNode } from 'react';
import { waitFor, screen, render } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { MemoryRouter } from 'react-router';
import { setupServer } from 'msw/node';
import { vi } from 'vitest';
import { AppContext, defaultAppState } from '@hasura/shared/context';
import {
  fetchGithubMetadataHandler,
  metadataFailureHandler,
  metadataSuccessHandler,
  mockGithubServerDownHandler,
} from '../mocks/handlers.mock';
import { useInstallMetadata } from './useInstallMetadata';
import Endpoints from '../../../../Endpoints';
import { mockMetadataUrl, serverDownErrorMessage } from '../mocks/constants';

const server = setupServer();

function waitForRequest(method: string, url: string) {
  let requestId = '';
  return new Promise<Request>((resolve, reject) => {
    server.events.on('request:start', async ({ request, requestId: id }) => {
      const matchesMethod =
        request.method.toLowerCase() === method.toLowerCase();
      const matchesUrl = request.url === url || request.url.startsWith(url);
      let matchesType = false;

      try {
        const reqbody = await request.clone().json();
        matchesType = reqbody?.type === 'replace_metadata';
      } catch (err) {
        // not a metadata request, can be ignored
      }

      if (matchesMethod && matchesUrl && matchesType) {
        requestId = id;
      }
    });
    server.events.on('request:match', ({ request, requestId: id }) => {
      if (id === requestId) {
        resolve(request);
      }
    });
    server.events.on('request:unhandled', ({ request, requestId: id }) => {
      if (id === requestId) {
        reject(
          new Error(
            `The ${request.method} ${request.url} request was unhandled.`,
          ),
        );
      }
    });
  });
}

let reactQueryClient = new QueryClient();

beforeAll(() => server.listen({ onUnhandledFrame: 'warn' }));
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

const onSuccessCb = vi.fn(() => {});

const onErrorCb = vi.fn(() => {});

const Component = () => {
  const { updateMetadata } = useInstallMetadata(
    'default',
    mockMetadataUrl,
    onSuccessCb,
    onErrorCb,
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

const wrapper = ({ children }: Props) => (
  <QueryClientProvider client={reactQueryClient}>
    <MemoryRouter>
      <AppContext.Provider value={{ ...defaultAppState, endpoints: Endpoints }}>
        {children}
      </AppContext.Provider>
    </MemoryRouter>
  </QueryClientProvider>
);

describe('Check useInstallMetadata installs the correct metadata', () => {
  it('should install the correct metadata and call success callback', async () => {
    server.use(fetchGithubMetadataHandler, metadataSuccessHandler);
    const pendingRequest = waitForRequest('POST', Endpoints.metadata);

    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.getByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect success callback to be called, after successful `replace_metadata` request
    await waitFor(() => expect(onSuccessCb).toHaveBeenCalledTimes(1));

    // STEP 3: expect the correct metadata being sent to the server
    const replaceMetadataRequest = await pendingRequest;
    expect(await replaceMetadataRequest.clone().json()).toMatchSnapshot();
  });

  it('fails to fetch metadata file from github, should call the error callback', async () => {
    server.use(
      mockGithubServerDownHandler(mockMetadataUrl),
      metadataSuccessHandler,
    );
    render(<Component />, { wrapper });

    // STEP 1: expect our mock component renders successfully
    expect(screen.getByText('Welcome')).toBeInTheDocument();

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
    expect(screen.getByText('Welcome')).toBeInTheDocument();

    // STEP 2: expect error callback to be called, after applying metadata to server fails
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledTimes(1));

    // STEP 3: expect error callback to be called with correct arguments
    const errorMessage = JSON.stringify(serverDownErrorMessage);
    await waitFor(() => expect(onErrorCb).toHaveBeenCalledWith(errorMessage));
  });
});
