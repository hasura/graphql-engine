import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { UniqueKeys } from './UniqueKeys';

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

const createUniqueKey = vi.fn();
const dropUniqueKey = vi.fn().mockResolvedValue(true);
const confirmSpy = vi.fn(
  ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => onConfirm(),
);
let addSupported = true;
let dropSupported = true;
let existing = [{ constraintName: 'orders_email_key', columns: ['email'] }];

vi.mock('@hasura/metadata/data-source', () => ({
  useCreateUniqueKey: () => ({ mutate: createUniqueKey, isPending: false }),
  useDropUniqueKey: () => ({ mutateAsync: dropUniqueKey }),
  useTableUniqueKeys: () => ({ data: existing, isLoading: false }),
  useTableColumns: () => ({ data: { columns: [{ name: 'email' }] } }),
  getDatabaseMethods: () => ({
    introspection: { getUniqueKeys: () => undefined },
    modify: {
      ...(addSupported ? { createUniqueKey: () => undefined } : {}),
      ...(dropSupported ? { dropUniqueKey: () => undefined } : {}),
    },
  }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return { ...actual, useDestructiveConfirm: () => confirmSpy };
});

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { table: { name: 'orders', schema: 'public' } } as MetadataTable;

const setup = () =>
  render(
    <Theme>
      <UniqueKeys source={source} table={table} isView={false} />
    </Theme>,
  );

describe('ModifyTable UniqueKeys', () => {
  beforeEach(() => {
    createUniqueKey.mockClear();
    dropUniqueKey.mockClear();
    confirmSpy.mockClear();
    addSupported = true;
    dropSupported = true;
    existing = [{ constraintName: 'orders_email_key', columns: ['email'] }];
  });

  it('lists existing unique keys and the add button', () => {
    setup();
    expect(screen.getByText('orders_email_key')).toBeInTheDocument();
    expect(
      screen.getByRole('button', { name: /add unique key/i }),
    ).toBeInTheDocument();
  });

  it('removes a unique key via confirmation, passing columns for the down migration', async () => {
    setup();
    await userEvent.click(screen.getByRole('button', { name: /remove/i }));
    expect(confirmSpy).toHaveBeenCalledTimes(1);
    await waitFor(() =>
      expect(dropUniqueKey).toHaveBeenCalledWith(
        expect.objectContaining({
          constraintName: 'orders_email_key',
          columns: ['email'],
          table: table.table,
        }),
      ),
    );
  });

  it('hides the add button when the driver lacks createUniqueKey', () => {
    addSupported = false;
    setup();
    expect(
      screen.queryByRole('button', { name: /add unique key/i }),
    ).not.toBeInTheDocument();
  });
});
