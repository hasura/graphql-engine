import { render, screen } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { MemoryRouter } from 'react-router';
import type { Source, Table } from '@hasura/shared/types';
import { TableName } from './TableName';

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

let dropTableSupported = true;

vi.mock('@hasura/metadata/api', () => ({
  useUntrackTable: () => ({ mutateAsync: vi.fn(), isPending: false }),
}));

vi.mock('@hasura/metadata/data-source', () => ({
  useDropTable: () => ({ mutate: vi.fn(), isPending: false }),
  getDatabaseMethods: () => ({
    modify: dropTableSupported ? { dropTable: () => undefined } : {},
  }),
}));

vi.mock('@hasura/metadata/helpers', () => ({
  isNativeDriver: () => false, // avoids rendering CreateRestEndpoint + its deps
}));

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { name: 'orders', schema: 'public' } as unknown as Table;

const setup = () =>
  render(
    <Theme>
      <MemoryRouter>
        <TableName source={source} table={table} tableName="orders" />
      </MemoryRouter>
    </Theme>,
  );

describe('ManageTable TableName untrack/delete', () => {
  beforeEach(() => {
    dropTableSupported = true;
  });

  it('offers Untrack and Delete when the driver supports dropTable', async () => {
    setup();
    await userEvent.click(screen.getByRole('button', { name: /orders/i }));
    expect(screen.getByText('Untrack')).toBeInTheDocument();
    expect(screen.getByText('Delete')).toBeInTheDocument();
  });

  it('hides Delete when the driver does not support dropTable', async () => {
    dropTableSupported = false;
    setup();
    await userEvent.click(screen.getByRole('button', { name: /orders/i }));
    expect(screen.getByText('Untrack')).toBeInTheDocument();
    expect(screen.queryByText('Delete')).not.toBeInTheDocument();
  });
});
