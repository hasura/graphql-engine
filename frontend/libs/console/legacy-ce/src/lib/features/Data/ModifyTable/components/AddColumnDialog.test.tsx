import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { HttpError } from '@hasura/shared/types';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { AppContext, AppState, defaultAppState } from '@hasura/shared/context';
import { AddColumnDialog } from './AddColumnDialog';

class ResizeObserverStub {
  observe() {
    /* no-op */
  }
  unobserve() {
    /* no-op */
  }
  disconnect() {
    /* no-op */
  }
}
globalThis.ResizeObserver =
  globalThis.ResizeObserver ?? (ResizeObserverStub as never);

const addColumn = vi.fn();
const triggerSql = vi.fn(() => ({ upSql: 'up', downSql: 'down' }));
const mutationState: { isPending: boolean; error: unknown } = {
  isPending: false,
  error: null,
};

vi.mock('@hasura/metadata/data-source', () => ({
  useSupportedDataTypes: () => ({ data: { text: ['text'] } }),
  useAddColumn: () => ({ mutate: addColumn, ...mutationState }),
  getDatabaseMethods: () => ({
    config: {
      getFrequentlyUsedColumns: () => [
        {
          name: 'id',
          validFor: ['add'],
          type: 'serial',
          typeText: 'serial',
        },
        {
          name: 'updated_at',
          validFor: ['add', 'modify'],
          type: 'timestamptz',
          typeText: 'timestamp',
          default: 'now()',
          dependentSQLGenerator: triggerSql,
        },
      ],
    },
  }),
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = {
  table: { name: 'Album', schema: 'public' },
} as MetadataTable;

const findInput = (name: string) =>
  waitFor(() => {
    const el = document.querySelector<HTMLInputElement>(
      `input[name="${name}"]`,
    );
    if (!el) throw new Error(`input "${name}" not rendered`);
    return el;
  });

const renderInMode = (consoleMode: 'cli' | 'server', onClose = vi.fn()) =>
  render(
    <AppContext.Provider
      value={{
        ...defaultAppState,
        envVars: { consoleMode } as AppState['envVars'],
      }}
    >
      <AddColumnDialog source={source} table={table} onClose={onClose} />
    </AppContext.Provider>,
  );

describe('AddColumnDialog', () => {
  beforeEach(() => {
    addColumn.mockClear();
    mutationState.isPending = false;
    mutationState.error = null;
  });

  it('explains a pending CLI request and cannot be closed meanwhile', async () => {
    mutationState.isPending = true;
    renderInMode('cli');
    expect(
      await screen.findByText(/writing its migration through the Hasura CLI/),
    ).toBeVisible();
    // `Dialog` only renders its header close button (in this wrapper) when it
    // is given `onClose`
    expect(document.querySelector('.absolute.right-6')).toBeNull();
    expect(screen.getByRole('button', { name: 'Cancel' })).toBeDisabled();
  });

  it('warns that a CLI failure after the SQL ran may have applied it', async () => {
    mutationState.error = new HttpError(500, 'Internal Server Error', {
      code: 'internal_error',
      message: 'cannot set version',
    });
    renderInMode('cli');
    expect(
      await screen.findByText('The change may already be in the database'),
    ).toBeVisible();
    expect(screen.getByText('hasura migrate status')).toBeVisible();
  });

  it('does not warn when the database rejected the SQL', async () => {
    mutationState.error = new HttpError(400, 'Bad Request', {
      code: 'data_api_error',
      message: 'column "a" already exists',
    });
    renderInMode('cli');
    expect(await screen.findByText('Adding column failed')).toBeVisible();
    // not pending: the header close button is back
    expect(document.querySelector('.absolute.right-6')).not.toBeNull();
    expect(
      screen.queryByText('The change may already be in the database'),
    ).not.toBeInTheDocument();
  });

  it('only offers presets valid for modifying a table', async () => {
    render(<AddColumnDialog source={source} table={table} onClose={vi.fn()} />);
    expect(
      await screen.findByRole('button', { name: 'updated_at' }),
    ).toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: 'id' }),
    ).not.toBeInTheDocument();
  });

  it('adds a preset column with its dependent SQL', async () => {
    render(<AddColumnDialog source={source} table={table} onClose={vi.fn()} />);
    await userEvent.click(
      await screen.findByRole('button', { name: 'updated_at' }),
    );
    expect(await findInput('name')).toHaveValue('updated_at');
    await userEvent.click(screen.getByRole('button', { name: 'Add Column' }));

    await waitFor(() => expect(addColumn).toHaveBeenCalled());
    expect(addColumn).toHaveBeenCalledWith({
      source: { name: 'default', kind: 'postgres' },
      table: table.table,
      column: {
        name: 'updated_at',
        type: 'timestamptz',
        nullable: true,
        unique: false,
        default: { value: 'now()' },
        dependentSQLGenerator: triggerSql,
      },
    });
  });

  it('requires a name and a type', async () => {
    render(<AddColumnDialog source={source} table={table} onClose={vi.fn()} />);
    await findInput('name');
    await userEvent.click(screen.getByRole('button', { name: 'Add Column' }));
    expect(await screen.findByText('Column name is required')).toBeVisible();
    expect(addColumn).not.toHaveBeenCalled();
  });
});
