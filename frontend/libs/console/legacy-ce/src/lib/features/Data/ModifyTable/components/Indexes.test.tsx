import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { Indexes } from './Indexes';

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

const createIndex = vi.fn();
const dropIndex = vi.fn().mockResolvedValue(true);
const confirmSpy = vi.fn(
  ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => onConfirm(),
);
let addSupported = true;
let existing = [
  { name: 'orders_email_idx', type: 'btree', columns: ['email'] },
];

vi.mock('@hasura/metadata/data-source', () => ({
  useCreateIndex: () => ({ mutate: createIndex, isPending: false }),
  useDropIndex: () => ({ mutateAsync: dropIndex }),
  useTableIndexes: () => ({ data: existing, isLoading: false }),
  useTableColumns: () => ({ data: { columns: [{ name: 'email' }] } }),
  getDatabaseMethods: () => ({
    introspection: { getTableIndexes: () => undefined },
    modify: {
      ...(addSupported ? { createIndex: () => undefined } : {}),
      dropIndex: () => undefined,
    },
  }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return { ...actual, useDestructiveConfirm: () => confirmSpy };
});

// Stub the dialog so onSubmit can be driven without react-select interaction.
vi.mock('./IndexForm', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return {
    ...actual,
    IndexForm: (props: { onSubmit: (v: unknown) => void }) => (
      <div data-testid="index-form">
        <button
          onClick={() =>
            props.onSubmit({
              name: 'orders_email_key',
              type: 'hash',
              columns: ['email'],
              unique: true,
            })
          }
        >
          submit
        </button>
      </div>
    ),
  };
});

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { table: { name: 'orders', schema: 'public' } } as MetadataTable;

const setup = () =>
  render(
    <Theme>
      <Indexes source={source} table={table} isView={false} />
    </Theme>,
  );

describe('ModifyTable Indexes', () => {
  beforeEach(() => {
    createIndex.mockClear();
    dropIndex.mockClear();
    confirmSpy.mockClear();
    addSupported = true;
    existing = [
      { name: 'orders_email_idx', type: 'btree', columns: ['email'] },
    ];
  });

  it('lists existing indexes', () => {
    setup();
    expect(screen.getByText('orders_email_idx')).toBeInTheDocument();
    expect(screen.queryByTestId('index-form')).not.toBeInTheDocument();
  });

  it('creates an index from the add dialog', async () => {
    setup();
    await userEvent.click(screen.getByRole('button', { name: /add index/i }));
    expect(screen.getByTestId('index-form')).toBeInTheDocument();
    await userEvent.click(screen.getByRole('button', { name: 'submit' }));
    expect(createIndex).toHaveBeenCalledWith({
      source: { name: 'default', kind: 'postgres' },
      table: table.table,
      indexName: 'orders_email_key',
      indexType: 'hash',
      columns: ['email'],
      unique: true,
    });
  });

  it('removes an index via confirmation', async () => {
    setup();
    await userEvent.click(
      screen.getByRole('button', { name: 'Remove index orders_email_idx' }),
    );
    expect(confirmSpy).toHaveBeenCalledTimes(1);
    await waitFor(() =>
      expect(dropIndex).toHaveBeenCalledWith(
        expect.objectContaining({
          indexName: 'orders_email_idx',
          table: table.table,
        }),
      ),
    );
  });

  it('hides the add button when the driver lacks createIndex', () => {
    addSupported = false;
    setup();
    expect(
      screen.queryByRole('button', { name: /add index/i }),
    ).not.toBeInTheDocument();
  });
});
