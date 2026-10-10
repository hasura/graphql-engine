import { render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { AddTable } from './AddTable';

const renderContainer = () =>
  render(
    <MemoryRouter>
      <AddTable />
    </MemoryRouter>,
  );

let currentSource: { name: string; kind: string } = {
  name: 'default',
  kind: 'postgres',
};
let schemaParam: string | null = 'public';

vi.mock('react-router', async (importOriginal) => {
  const actual = (await importOriginal()) as typeof import('react-router');
  return {
    ...actual,
    useNavigate: () => vi.fn(),
    useSearchParams: () => [{ get: () => schemaParam }] as never,
  };
});

vi.mock('../context/DataSourceContext', () => ({
  useDataSourceContext: () => ({ currentSource }),
}));

vi.mock('@hasura/metadata/data-source', () => ({
  getDatabaseMethods: (kind: string) => ({
    modify: kind === 'postgres' ? { createTable: () => undefined } : {},
  }),
}));

// Stub the form so the container test stays focused on routing/gating.
vi.mock('./AddTableForm', () => ({
  AddTableForm: (props: {
    source: { kind: string };
    targetSchema: string | null;
  }) => (
    <div data-testid="add-table-form">
      {props.source.kind}:{String(props.targetSchema)}
    </div>
  ),
}));

describe('AddTable (route container)', () => {
  beforeEach(() => {
    currentSource = { name: 'default', kind: 'postgres' };
    schemaParam = 'public';
  });

  it('renders the form for a driver that supports table creation (postgres)', () => {
    renderContainer();
    expect(screen.getByTestId('add-table-form')).toHaveTextContent(
      'postgres:public',
    );
  });

  it('passes a null target schema through when none is in the URL', () => {
    schemaParam = null;
    renderContainer();
    expect(screen.getByTestId('add-table-form')).toHaveTextContent(
      'postgres:null',
    );
  });

  it('shows an unsupported message for drivers without createTable (mssql)', () => {
    currentSource = { name: 'mssql_db', kind: 'mssql' };
    renderContainer();
    expect(
      screen.getByText(/adding tables is not supported/i),
    ).toBeInTheDocument();
    expect(screen.queryByTestId('add-table-form')).not.toBeInTheDocument();
  });
});
