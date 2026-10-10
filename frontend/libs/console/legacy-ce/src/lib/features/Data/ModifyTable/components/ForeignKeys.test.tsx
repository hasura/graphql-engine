import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { ForeignKeys } from './ForeignKeys';

const dropForeignKey = vi.fn().mockResolvedValue(true);
const confirmSpy = vi.fn(
  ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => onConfirm(),
);

let createSupported = true;
let dropSupported = true;
let alterSupported = true;

const existingFk = {
  name: 'orders_customer_id_fkey',
  from: {
    table: { name: 'orders', schema: 'public' },
    columns: ['customer_id'],
  },
  to: { table: { name: 'customers', schema: 'public' }, columns: ['id'] },
  onUpdate: 'restrict',
  onDelete: 'restrict',
};

vi.mock('@hasura/metadata/data-source', () => ({
  useTableForeignKeys: () => ({
    data: [existingFk],
    isLoading: false,
    isError: false,
  }),
  useDropForeignKey: () => ({ mutateAsync: dropForeignKey }),
  generateForeignKeyLabel: () => 'orders.customer_id → customers.id',
  getDatabaseMethods: () => ({
    modify: {
      ...(createSupported ? { createForeignKey: () => undefined } : {}),
      ...(dropSupported ? { dropForeignKey: () => undefined } : {}),
      ...(alterSupported ? { alterForeignKey: () => undefined } : {}),
    },
  }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return { ...actual, useDestructiveConfirm: () => confirmSpy };
});

// Isolate: the add/edit dialogs are covered by their own tests.
vi.mock('./ForeignKeyAddForm', () => ({
  ForeignKeyAddForm: () => <div data-testid="fk-add-form" />,
}));
vi.mock('./ForeignKeyEditForm', () => ({
  ForeignKeyEditForm: () => <div data-testid="fk-edit-form" />,
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { table: { name: 'orders', schema: 'public' } } as MetadataTable;

describe('ModifyTable ForeignKeys', () => {
  beforeEach(() => {
    dropForeignKey.mockClear();
    confirmSpy.mockClear();
    createSupported = true;
    dropSupported = true;
    alterSupported = true;
  });

  it('lists existing foreign keys and opens the add dialog when supported', async () => {
    render(<ForeignKeys source={source} table={table} isView={false} />);
    expect(
      screen.getByText('orders.customer_id → customers.id'),
    ).toBeInTheDocument();
    expect(screen.queryByTestId('fk-add-form')).not.toBeInTheDocument();
    await userEvent.click(
      screen.getByRole('button', { name: /add foreign key/i }),
    );
    expect(screen.getByTestId('fk-add-form')).toBeInTheDocument();
  });

  it('drops a foreign key through the confirmation flow', async () => {
    render(<ForeignKeys source={source} table={table} isView={false} />);
    await userEvent.click(screen.getByRole('button', { name: /remove/i }));

    expect(confirmSpy).toHaveBeenCalledTimes(1);
    await waitFor(() => expect(dropForeignKey).toHaveBeenCalledTimes(1));
    expect(dropForeignKey).toHaveBeenCalledWith(
      expect.objectContaining({
        source: { name: 'default', kind: 'postgres' },
        constraintName: 'orders_customer_id_fkey',
        from: existingFk.from,
        to: existingFk.to,
      }),
    );
  });

  it('hides the remove button when the driver lacks dropForeignKey', () => {
    dropSupported = false;
    render(<ForeignKeys source={source} table={table} isView={false} />);
    expect(
      screen.queryByRole('button', { name: /remove/i }),
    ).not.toBeInTheDocument();
  });

  it('hides the add button when the driver lacks createForeignKey', () => {
    createSupported = false;
    render(<ForeignKeys source={source} table={table} isView={false} />);
    expect(
      screen.queryByRole('button', { name: /add foreign key/i }),
    ).not.toBeInTheDocument();
  });

  it('opens the edit dialog when the driver supports alterForeignKey', async () => {
    render(<ForeignKeys source={source} table={table} isView={false} />);
    expect(screen.queryByTestId('fk-edit-form')).not.toBeInTheDocument();
    await userEvent.click(screen.getByRole('button', { name: /edit/i }));
    expect(screen.getByTestId('fk-edit-form')).toBeInTheDocument();
  });

  it('hides the edit button when the driver lacks alterForeignKey', () => {
    alterSupported = false;
    render(<ForeignKeys source={source} table={table} isView={false} />);
    expect(
      screen.queryByRole('button', { name: /^edit/i }),
    ).not.toBeInTheDocument();
  });
});
