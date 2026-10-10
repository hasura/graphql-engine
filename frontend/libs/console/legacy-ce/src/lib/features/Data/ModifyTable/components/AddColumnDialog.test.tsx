import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import type { MetadataTable, Source } from '@hasura/shared/types';
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

vi.mock('@hasura/metadata/data-source', () => ({
  useSupportedDataTypes: () => ({ data: { text: ['text'] } }),
  useAddColumn: () => ({ mutate: addColumn, isPending: false, error: null }),
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

describe('AddColumnDialog', () => {
  beforeEach(() => addColumn.mockClear());

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
