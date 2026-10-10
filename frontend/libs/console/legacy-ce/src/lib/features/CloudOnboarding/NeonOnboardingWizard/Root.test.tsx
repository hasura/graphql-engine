import { ReactNode } from 'react';
import { render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { setupServer } from 'msw/node';
import {
  mutationBaseHandlers,
  fetchGithubMigrationHandler,
  fetchGithubMetadataHandler,
  metadataSuccessHandler,
  querySuccessHandler,
  mockGithubServerDownHandler,
  onboardingDataEmptyActivity,
  fetchOnboardingDataFailure,
  onboardingDataSkippedOnboarding,
  onboardingDataCompleteOnboarding,
  onboardingDataHasuraSourceCreationStart,
  onboardingDataRunQueryClick,
  fetchAnsweredSurveysHandler,
} from './mocks/handlers.mock';
import { mockSampleQueryUrl, mockSchemaImageUrl } from './mocks/constants';
import { Root } from './Root';

const server = setupServer(
  ...mutationBaseHandlers(),
  fetchAnsweredSurveysHandler,
  mockGithubServerDownHandler(mockSampleQueryUrl),
  mockGithubServerDownHandler(mockSchemaImageUrl),
  fetchGithubMigrationHandler,
  fetchGithubMetadataHandler,
  metadataSuccessHandler,
  querySuccessHandler,
);

let reactQueryClient = new QueryClient();

beforeAll(() => {
  server.listen({ onUnhandledFrame: 'warn' });
});
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
afterEach(() => {
  server.resetHandlers();
});
afterAll(() => server.close());

type Props = {
  children?: ReactNode;
};

const wrapper = ({ children }: Props) => (
  <QueryClientProvider client={reactQueryClient}>
    {children}
  </QueryClientProvider>
);

describe('Check different configurations of Onboarding wizard depending on onboarding data', () => {
  it('should hide wizard as onboarding data is not present', async () => {
    server.use(fetchOnboardingDataFailure);
    render(<Root />, { wrapper });

    await waitFor(() =>
      expect(
        screen.queryByText('Welcome to your new Hasura project!'),
      ).not.toBeInTheDocument(),
    );
  });

  it('should not show wizard', async () => {
    server.use(onboardingDataEmptyActivity);
    render(<Root />, { wrapper });

    await waitFor(() =>
      expect(
        screen.queryByText('Welcome to your new Hasura project!'),
      ).not.toBeInTheDocument(),
    );
  });

  it('should hide wizard as onboarding skipped by user', async () => {
    server.use(onboardingDataSkippedOnboarding);
    render(<Root />, { wrapper });

    await waitFor(() =>
      expect(
        screen.queryByText('Welcome to your new Hasura project!'),
      ).not.toBeInTheDocument(),
    );
  });

  it('should hide wizard as onboarding completed by user', async () => {
    server.use(onboardingDataCompleteOnboarding);
    render(<Root />, { wrapper });

    await waitFor(() =>
      expect(
        screen.queryByText('Welcome to your new Hasura project!'),
      ).not.toBeInTheDocument(),
    );
  });

  it('should hide wizard as hasura data source creation started', async () => {
    server.use(onboardingDataHasuraSourceCreationStart);
    render(<Root />, { wrapper });

    await waitFor(() =>
      expect(
        screen.queryByText('Welcome to your new Hasura project!'),
      ).not.toBeInTheDocument(),
    );
  });

  it('should hide wizard as run query button clicked', async () => {
    server.use(onboardingDataRunQueryClick);
    render(<Root />, { wrapper });

    await waitFor(() =>
      expect(
        screen.queryByText('Welcome to your new Hasura project!'),
      ).not.toBeInTheDocument(),
    );
  });
});
