import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import type { Source } from '@hasura/shared/types';
import { ForeignKeyAddForm } from './ForeignKeyAddForm';

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

const createForeignKey = vi.fn();

vi.mock('@hasura/metadata/data-source', () => ({
  useCreateForeignKey: () => ({ mutate: createForeignKey, isPending: false }),
  useTableColumns: () => ({ data: { columns: [{ name: 'id' }] } }),
  createTableSelectOption: (table: { name: string; schema: string }) => ({
    label: `${table.schema} / ${table.name}`,
    value: table,
  }),
  getDatabaseMethods: () => ({
    config: { getViolationActions: () => ['restrict', 'cascade'] },
  }),
}));

vi.mock('@hasura/metadata/api', () => ({
  useMetadata: () => ({
    data: [{ table: { name: 'customers', schema: 'public' } }],
  }),
}));

vi.mock('@hasura/metadata/helpers', () => ({
  MetadataSelectors: { getTables: () => (m: unknown) => m },
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { name: 'orders', schema: 'public' };

const setup = () =>
  render(
    <Theme>
      <ForeignKeyAddForm source={source} table={table} onClose={vi.fn()} />
    </Theme>,
  );

describe('ForeignKeyAddForm', () => {
  beforeEach(() => createForeignKey.mockClear());

  it('renders the reference-table picker, mapping row and cascade selects', async () => {
    setup();
    // The form lives in a DelayedDialog that mounts its content after a tick.
    expect(await screen.findByText('Reference table')).toBeInTheDocument();
    expect(screen.getByText('From column')).toBeInTheDocument();
    expect(screen.getByText('To column')).toBeInTheDocument();
    expect(screen.getByText('On update')).toBeInTheDocument();
    expect(screen.getByText('On delete')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /add foreign key/i }),
    ).toBeInTheDocument();
  });

  it('does not submit when no reference table / columns are chosen', async () => {
    setup();
    await userEvent.click(
      await screen.findByRole('button', { name: /add foreign key/i }),
    );
    // zod validation should block the empty form
    await waitFor(() => expect(createForeignKey).not.toHaveBeenCalled());
  });
});
