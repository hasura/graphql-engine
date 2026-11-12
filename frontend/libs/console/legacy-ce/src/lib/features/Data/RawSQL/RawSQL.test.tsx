import { useRef } from 'react';
import { fireEvent, render, screen } from '@testing-library/react';
import { MemoryRouter } from 'react-router';
import { Theme } from '@radix-ui/themes';
import { AppContext, defaultAppState } from '@hasura/shared/context';
import { useMetadata, useMigrationStatus } from '@hasura/metadata/api';
import RawSQL from './RawSQL';
import { useRunRawSQL } from './hooks/useRunRawSQL';

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: vi.fn(),
  useMigrationStatus: vi.fn(() => ({ data: undefined })),
}));

vi.mock('./hooks/useRunRawSQL', () => ({
  useRunRawSQL: vi.fn(),
}));

// Ace doesn't run in jsdom. Like react-ace, the mock registers `commands` only
// on mount, and exposes the submit shortcut as a button.
vi.mock('react-ace', async (importActual) => ({
  // the real module loads the `ace` global the editor extensions need
  ...(await importActual<object>()),
  default: function AceMock({
    value,
    onChange,
    commands,
  }: {
    value: string;
    onChange: (value: string) => void;
    commands?: { name: string; exec: (editor: unknown) => void }[];
  }) {
    const mountedCommands = useRef(commands);
    const submit = mountedCommands.current?.find(
      (command) => command.name === 'submit',
    );
    return (
      <>
        <textarea
          aria-label="SQL"
          value={value}
          onChange={(event) => onChange(event.target.value)}
        />
        <button
          type="button"
          onClick={() =>
            submit?.exec({ getValue: () => value } as unknown as never)
          }
        >
          Ctrl+Enter
        </button>
      </>
    );
  },
}));

// The Radix select can't be driven in jsdom; a native one keeps the wiring.
vi.mock('./DropDownSelector', () => ({
  default: ({
    options,
    value,
    onChange,
  }: {
    options: { name: string }[];
    value?: string;
    onChange: (value: string) => void;
  }) => (
    <select
      aria-label="Database"
      value={value ?? ''}
      onChange={(event) => onChange(event.target.value)}
    >
      <option value="" disabled />
      {options.map(({ name }) => (
        <option key={name} value={name}>
          {name}
        </option>
      ))}
    </select>
  ),
}));

const metadata = {
  metadata: {
    version: 3,
    sources: [
      { name: 'default', kind: 'postgres', tables: [] },
      { name: 'warehouse', kind: 'mssql', tables: [] },
      { name: 'cluster', kind: 'citus', tables: [] },
    ],
  },
};

const mutate = vi.fn();
const mockedUseMetadata = vi.mocked(useMetadata);

const setMetadata = (data: typeof metadata | undefined) =>
  mockedUseMetadata.mockReturnValue({ data } as ReturnType<typeof useMetadata>);

const renderRawSQL = (
  path = '/data/sql',
  consoleMode: 'server' | 'cli' = 'server',
) => {
  const ui = () => (
    <Theme>
      <MemoryRouter initialEntries={[path]}>
        <AppContext.Provider
          value={{
            ...defaultAppState,
            envVars: {
              consoleMode,
            } as typeof defaultAppState.envVars,
          }}
        >
          <RawSQL />
        </AppContext.Provider>
      </MemoryRouter>
    </Theme>
  );
  const view = render(ui());
  return { ...view, rerender: () => view.rerender(ui()) };
};

const database = () => screen.getByLabelText('Database') as HTMLSelectElement;

const typeSQL = (sql: string) =>
  fireEvent.change(screen.getByLabelText('SQL'), { target: { value: sql } });

const lastRun = () => mutate.mock.lastCall?.[0];

beforeEach(() => {
  localStorage.clear();
  mutate.mockReset();
  vi.mocked(useMigrationStatus).mockReturnValue({
    data: undefined,
  } as ReturnType<typeof useMigrationStatus>);
  vi.mocked(useRunRawSQL).mockReturnValue({
    mutate,
    data: undefined,
    isPending: false,
  } as unknown as ReturnType<typeof useRunRawSQL>);
  setMetadata(metadata);
});

describe('RawSQL', () => {
  it('runs against the database picked in the dropdown', () => {
    renderRawSQL();
    expect(database().value).toBe('default');

    fireEvent.change(database(), { target: { value: 'warehouse' } });
    expect(database().value).toBe('warehouse');

    typeSQL('select 1');
    fireEvent.click(screen.getByText('Run!'));

    expect(lastRun()).toMatchObject({
      source: { name: 'warehouse', kind: 'mssql' },
      sql: 'select 1',
    });
  });

  it('selects the first database once the metadata has loaded', () => {
    setMetadata(undefined);
    const { rerender } = renderRawSQL();
    expect(screen.getByText('Run!').closest('button')).toBeDisabled();

    setMetadata(metadata);
    rerender();

    expect(database().value).toBe('default');
    typeSQL('select 1');
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ source: { name: 'default' } });
  });

  it('selects the database from the `source` param, or falls back', () => {
    const { unmount } = renderRawSQL('/data/sql?source=warehouse');
    expect(database().value).toBe('warehouse');
    unmount();

    renderRawSQL('/data/sql?source=missing');
    expect(database().value).toBe('default');
  });

  it('submits the current state from the keyboard shortcut', () => {
    setMetadata(undefined);
    const { rerender } = renderRawSQL();
    setMetadata(metadata);
    rerender();

    fireEvent.change(database(), { target: { value: 'warehouse' } });
    typeSQL('select 2');
    fireEvent.click(screen.getByText('Ctrl+Enter'));

    expect(lastRun()).toMatchObject({
      source: { name: 'warehouse', kind: 'mssql' },
      sql: 'select 2',
    });
  });

  it('keeps the statement timeout across drivers without support for it', () => {
    renderRawSQL();
    const timeout = () =>
      screen.queryByDisplayValue('25') as HTMLInputElement | null;

    fireEvent.change(screen.getByDisplayValue('10'), {
      target: { value: '25' },
    });
    expect(timeout()).toBeInTheDocument();

    fireEvent.change(database(), { target: { value: 'warehouse' } });
    expect(timeout()).not.toBeInTheDocument();
    typeSQL('select 1');
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ statementTimeout: null });

    fireEvent.change(database(), { target: { value: 'default' } });
    expect(timeout()).toBeInTheDocument();
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ statementTimeout: 25 });
  });

  it('only sends "Track this" while it applies', () => {
    renderRawSQL();
    const track = () => screen.getByRole('checkbox', { name: /Track this/ });

    typeSQL('create table foo (id int)');
    expect(track()).toBeChecked();
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ isTableTracked: true });

    // no longer creating anything
    typeSQL('select 1');
    expect(track()).not.toBeChecked();
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ isTableTracked: false });

    // disabled for Citus until the SQL is plpgsql
    typeSQL('create table foo (id int)');
    fireEvent.change(database(), { target: { value: 'cluster' } });
    expect(track()).toBeDisabled();
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ isTableTracked: false });
  });

  it('only sends "No transaction" while it applies, without a timeout', () => {
    renderRawSQL();
    const noTransaction = () =>
      screen.queryByRole('checkbox', { name: /No transaction/ });
    const timeout = () => screen.queryByDisplayValue('10');

    typeSQL('create index concurrently idx on foo (id)');
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({
      noTransaction: false,
      statementTimeout: 10,
    });

    // the timeout is a `SET LOCAL`, which has no effect outside a transaction
    fireEvent.click(screen.getByRole('checkbox', { name: /No transaction/ }));
    expect(timeout()).not.toBeInTheDocument();
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({
      noTransaction: true,
      statementTimeout: null,
    });

    // SQL Server has no such option
    fireEvent.change(database(), { target: { value: 'warehouse' } });
    expect(noTransaction()).not.toBeInTheDocument();
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ noTransaction: false });

    // Citus does, and the choice is kept
    fireEvent.change(database(), { target: { value: 'cluster' } });
    expect(noTransaction()).toBeChecked();
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ noTransaction: true });

    fireEvent.change(database(), { target: { value: 'default' } });
    fireEvent.click(screen.getByRole('checkbox', { name: /No transaction/ }));
    expect(timeout()).toBeInTheDocument();
  });

  it('warns that a migration does not record "No transaction"', () => {
    vi.mocked(useMigrationStatus).mockReturnValue({
      data: 'healthy',
    } as ReturnType<typeof useMigrationStatus>);
    // migration files are only written in CLI mode
    renderRawSQL('/data/sql', 'cli');
    const note = () => screen.queryByText(/migrate apply --no-transaction/);

    typeSQL('create index concurrently idx on foo (id)');
    expect(
      screen.getByRole('checkbox', { name: /This is a migration/ }),
    ).toBeChecked();
    expect(note()).not.toBeInTheDocument();

    fireEvent.click(screen.getByRole('checkbox', { name: /No transaction/ }));
    expect(note()).toBeInTheDocument();
  });

  it("keeps the user's migration choice until the SQL implies another", () => {
    vi.mocked(useMigrationStatus).mockReturnValue({
      data: 'healthy',
    } as ReturnType<typeof useMigrationStatus>);
    renderRawSQL();
    const migration = () =>
      screen.getByRole('checkbox', { name: /This is a migration/ });

    typeSQL('create table foo (id int)');
    expect(migration()).toBeChecked();

    fireEvent.click(migration());
    typeSQL('create table foo (id int, name text)');
    expect(migration()).not.toBeChecked();
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ migration: undefined });

    typeSQL('select 1');
    fireEvent.click(migration());
    typeSQL('select 2');
    expect(migration()).toBeChecked();

    typeSQL('create table foo (id int)');
    typeSQL('select 1');
    expect(migration()).not.toBeChecked();
  });

  it('keeps a cleared statement timeout cleared after a reload', () => {
    const { unmount } = renderRawSQL();
    fireEvent.change(screen.getByDisplayValue('10'), {
      target: { value: '' },
    });
    unmount();

    renderRawSQL();
    expect(screen.queryByDisplayValue('10')).not.toBeInTheDocument();
    typeSQL('select 1');
    fireEvent.click(screen.getByText('Run!'));
    expect(lastRun()).toMatchObject({ statementTimeout: 0 });
  });
});
