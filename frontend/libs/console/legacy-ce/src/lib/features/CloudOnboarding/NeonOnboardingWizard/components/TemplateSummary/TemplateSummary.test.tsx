import { render, screen, waitFor } from '@testing-library/react';
import { QueryClient, QueryClientProvider } from '@tanstack/react-query';
import { TemplateSummary } from './TemplateSummary';

const fetchMock = vi.fn();
vi.mock('../../utils', () => ({
  fetchTemplateDataQueryFn: (...args: unknown[]) => fetchMock(...args),
}));
vi.mock('../../../utils', () => ({
  runQueryInGraphiQL: vi.fn(),
  emitOnboardingEvent: vi.fn(),
}));
// Render the resolved query so we can assert what TemplateSummary derives.
vi.mock('../../../components/QueryScreen/QueryScreen', () => ({
  QueryScreen: ({ query }: { query: string }) => (
    <div data-testid="query">{query}</div>
  ),
}));

const renderWithClient = () => {
  const client = new QueryClient({
    defaultOptions: { queries: { retry: false } },
  });
  return render(
    <QueryClientProvider client={client}>
      <TemplateSummary templateUrl="https://templates/x" dismiss={vi.fn()} />
    </QueryClientProvider>,
  );
};

describe('TemplateSummary', () => {
  beforeEach(() => {
    fetchMock.mockReset();
  });

  it('renders the default query while the sample query is loading', () => {
    // never resolves within this assertion window
    fetchMock.mockReturnValue(new Promise(() => undefined));
    renderWithClient();
    expect(screen.getByTestId('query')).toHaveTextContent(
      'lookupCustomerOrder',
    );
  });

  it('shows the fetched sample query once it resolves (derived from data)', async () => {
    fetchMock.mockResolvedValue('query fetched { id }');
    renderWithClient();

    // initial render falls back to the default query
    expect(screen.getByTestId('query')).toHaveTextContent(
      'lookupCustomerOrder',
    );

    await waitFor(() =>
      expect(screen.getByTestId('query')).toHaveTextContent(
        'query fetched { id }',
      ),
    );
  });

  it('keeps the default query if the fetched data is not a string', async () => {
    fetchMock.mockResolvedValue({ not: 'a string' });
    renderWithClient();

    await waitFor(() => expect(fetchMock).toHaveBeenCalled());
    expect(screen.getByTestId('query')).toHaveTextContent(
      'lookupCustomerOrder',
    );
  });
});
