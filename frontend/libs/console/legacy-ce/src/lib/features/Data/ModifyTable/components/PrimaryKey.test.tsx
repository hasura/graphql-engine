import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { PrimaryKey } from './PrimaryKey';

const dropPrimaryKey = vi.fn().mockResolvedValue(true);
const confirmSpy = vi.fn(
  ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => onConfirm(),
);
let existingPk: { constraintName: string; columns: string[] } | null = null;

vi.mock('@hasura/metadata/data-source', () => ({
  useCreatePrimaryKey: () => ({ mutate: vi.fn(), isPending: false }),
  useAlterPrimaryKey: () => ({ mutate: vi.fn(), isPending: false }),
  useDropPrimaryKey: () => ({ mutateAsync: dropPrimaryKey }),
  useTablePrimaryKey: () => ({ data: existingPk, isLoading: false }),
  useTableColumns: () => ({
    data: { columns: [{ name: 'id' }, { name: 'x' }] },
  }),
  getDatabaseMethods: () => ({
    introspection: { getPrimaryKey: () => undefined },
    modify: {
      createPrimaryKey: () => undefined,
      alterPrimaryKey: () => undefined,
      dropPrimaryKey: () => undefined,
    },
  }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return { ...actual, useDestructiveConfirm: () => confirmSpy };
});

// Isolate: only check which dialog opens, not its internals.
vi.mock('./PrimaryKeyForm', () => ({
  PrimaryKeyForm: ({ title }: { title: string }) => (
    <div data-testid="pk-form">{title}</div>
  ),
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { table: { name: 'orders', schema: 'public' } } as MetadataTable;

const setup = () =>
  render(<PrimaryKey source={source} table={table} isView={false} />);

describe('ModifyTable PrimaryKey', () => {
  beforeEach(() => {
    dropPrimaryKey.mockClear();
    confirmSpy.mockClear();
    existingPk = null;
  });

  it('shows "no primary key" and opens the add dialog when none exists', async () => {
    setup();
    expect(screen.getByText(/no primary key/i)).toBeInTheDocument();
    expect(screen.queryByTestId('pk-form')).not.toBeInTheDocument();
    await userEvent.click(
      screen.getByRole('button', { name: /add primary key/i }),
    );
    expect(screen.getByTestId('pk-form')).toHaveTextContent('Add Primary Key');
  });

  it('shows the current PK and opens the edit dialog when one exists', async () => {
    existingPk = { constraintName: 'orders_pkey', columns: ['id'] };
    setup();
    expect(screen.getByText('orders_pkey')).toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /add primary key/i }),
    ).not.toBeInTheDocument();
    await userEvent.click(
      screen.getByRole('button', { name: /edit primary key orders_pkey/i }),
    );
    expect(screen.getByTestId('pk-form')).toHaveTextContent('Edit orders_pkey');
  });

  it('drops the primary key through the confirmation flow', async () => {
    existingPk = { constraintName: 'orders_pkey', columns: ['id'] };
    setup();
    await userEvent.click(
      screen.getByRole('button', { name: /remove primary key orders_pkey/i }),
    );
    expect(confirmSpy).toHaveBeenCalledWith(
      expect.objectContaining({
        resourceName: 'orders_pkey',
        resourceType: 'Primary Key',
      }),
    );
    expect(dropPrimaryKey).toHaveBeenCalledWith(
      expect.objectContaining({
        constraintName: 'orders_pkey',
        columns: ['id'],
      }),
    );
  });
});
