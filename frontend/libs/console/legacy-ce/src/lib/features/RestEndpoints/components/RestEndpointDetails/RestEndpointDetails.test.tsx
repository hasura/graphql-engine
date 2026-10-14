import { render, screen, waitFor, act } from '@testing-library/react';
import { RestEndpointDetails } from './RestEndpointDetails';

const flush = () => act(async () => undefined);

// Regression for the set-state-in-effect / exhaustive-deps fixes:
//  - `getHeaders` is read once on mount via a ref (no refetch loop),
//  - editable `variables` are reset while rendering when the endpoint query
//    changes (instead of inside an effect),
//  - no runaway re-render.

const getHeadersSpy = vi.fn(() => Promise.resolve({ Authorization: 'secret' }));

let currentEndpoint: any = {
  endpoint: {
    name: 'ep',
    comment: null,
    url: '/x',
    methods: ['GET'],
    definition: { query: { collection_name: 'c' } },
  },
  query: { query: 'query Q($id: Int) { id }' },
};

vi.mock('@hasura/metadata/api', () => ({
  useRestEndpoint: () => currentEndpoint,
}));

vi.mock('../../hooks/useRestEndpointRequest', () => ({
  useRestEndpointRequest: () => ({
    data: undefined,
    mutate: vi.fn(),
    isPending: false,
  }),
}));

vi.mock('@hasura/shared/context', () => ({
  useAuthContext: () => ({ getHeaders: getHeadersSpy }),
}));

vi.mock('react-router', () => ({
  useNavigate: () => vi.fn(),
}));

vi.mock('@hasura/shared/analytics', () => ({
  Analytics: ({ children }: { children: React.ReactNode }) => <>{children}</>,
}));

vi.mock('../../utils', () => ({
  openInGraphiQL: vi.fn(),
}));

vi.mock('../../../ApiExplorer/components/Rest/utils', () => ({
  parseQueryVariables: (query: string) =>
    query.includes('$id') ? [{ name: 'id' }] : [{ name: 'other' }],
}));

vi.mock('../../../ApiExplorer/components/ApiRequest/utils', () => ({
  getPersistedGraphiQLHeaders: () => [
    { name: 'x-hasura-admin-secret', value: 'secret' },
  ],
}));

vi.mock('./Variables', () => ({
  Variables: ({ variables }: { variables: Array<{ name: string }> }) => (
    <div data-testid="variables">{variables.map((v) => v.name).join(',')}</div>
  ),
}));

vi.mock('./RequestHeaders', () => ({
  RequestHeaders: ({
    headers,
  }: {
    headers: Array<{ name: string; selected: boolean }>;
  }) => (
    <div data-testid="headers">
      {headers.map((h) => `${h.name}:${h.selected}`).join(',')}
    </div>
  ),
}));

const setValueSpy = vi.fn();

vi.mock('@hasura/shared/ui', () => ({
  useConsoleForm: () => ({
    Form: ({ children }: { children: React.ReactNode }) => (
      <form>{children}</form>
    ),
    methods: { setValue: setValueSpy },
  }),
  CheckboxesField: () => null,
  CodeEditorField: () => null,
  InputField: () => null,
  TextAreaField: () => null,
  Button: ({ children }: { children: React.ReactNode }) => (
    <button type="button">{children}</button>
  ),
  hasuraToast: vi.fn(),
  AceEditor: () => null,
}));

describe('RestEndpointDetails', () => {
  beforeEach(() => {
    getHeadersSpy.mockClear();
    setValueSpy.mockClear();
    currentEndpoint = {
      endpoint: {
        name: 'ep',
        comment: null,
        url: '/x',
        methods: ['GET'],
        definition: { query: { collection_name: 'c' } },
      },
      query: { query: 'query Q($id: Int) { id }' },
    };
  });

  it('seeds variables from the endpoint query on the initial render', async () => {
    render(<RestEndpointDetails name="ep" />);
    expect(screen.getByTestId('variables')).toHaveTextContent('id');
    await flush();
  });

  it('seeds request headers once on mount', async () => {
    render(<RestEndpointDetails name="ep" />);
    await waitFor(() => {
      expect(screen.getByTestId('headers')).toHaveTextContent(
        'x-hasura-admin-secret:true',
      );
    });
    // Mount-only fetch: getHeaders read exactly once (no refetch loop).
    expect(getHeadersSpy).toHaveBeenCalledTimes(1);
  });

  it('resets variables when the endpoint query changes', async () => {
    const { rerender } = render(<RestEndpointDetails name="ep" />);
    expect(screen.getByTestId('variables')).toHaveTextContent('id');

    currentEndpoint = {
      ...currentEndpoint,
      query: { query: 'query Q($other: Int) { other }' },
    };
    rerender(<RestEndpointDetails name="ep" />);
    expect(screen.getByTestId('variables')).toHaveTextContent('other');
    await flush();
  });

  it('does not re-read headers in a loop across re-renders', async () => {
    const { rerender } = render(<RestEndpointDetails name="ep" />);
    rerender(<RestEndpointDetails name="ep" />);
    rerender(<RestEndpointDetails name="ep" />);
    expect(getHeadersSpy).toHaveBeenCalledTimes(1);
    await flush();
  });
});
