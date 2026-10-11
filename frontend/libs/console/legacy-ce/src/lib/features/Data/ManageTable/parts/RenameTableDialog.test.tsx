import { act, render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import { MemoryRouter, Route, Routes, useLocation } from 'react-router';
import type { Source, Table } from '@hasura/shared/types';
import { RenameTableDialog } from './RenameTableDialog';

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

const changeTableName = vi.fn();
let onSuccess: ((renamed: boolean, vars: { newName: string }) => void) | null =
  null;

vi.mock('@hasura/metadata/data-source', () => ({
  useTrackableTables: () => ({ data: 'VIEW', isLoading: false }),
  useChangeTableName: (options: { onSuccess: typeof onSuccess }) => {
    onSuccess = options.onSuccess;
    return { mutate: changeTableName, isPending: false, error: null };
  },
}));

const source = {
  name: 'default',
  kind: 'postgres',
  tables: [],
} as unknown as Source;
const table = { name: 'orders', schema: 'public' } as unknown as Table;

const LocationProbe = () => {
  const location = useLocation();
  return (
    <div data-testid="location">{location.pathname + location.search}</div>
  );
};

const setup = () => {
  const onClose = vi.fn();
  render(
    <Theme>
      <MemoryRouter initialEntries={['/data/default/table/modify']}>
        <Routes>
          <Route
            path="/data/:database/table/:operation"
            element={
              <RenameTableDialog
                source={source}
                table={table}
                currentName="orders"
                onClose={onClose}
              />
            }
          />
          <Route path="*" element={null} />
        </Routes>
        <LocationProbe />
      </MemoryRouter>
    </Theme>,
  );
  return { onClose };
};

const getNameInput = () => screen.findByRole('textbox');

describe('RenameTableDialog', () => {
  beforeEach(() => {
    changeTableName.mockReset();
    onSuccess = null;
  });

  it('rejects an unchanged name', async () => {
    setup();
    await getNameInput();
    await userEvent.click(screen.getByRole('button', { name: 'Rename' }));
    expect(
      await screen.findByText('The name is already orders'),
    ).toBeInTheDocument();
    expect(changeTableName).not.toHaveBeenCalled();
  });

  it('rejects a name that is not a valid GraphQL name', async () => {
    setup();
    const input = await getNameInput();
    await userEvent.clear(input);
    await userEvent.type(input, '1 orders');
    await userEvent.click(screen.getByRole('button', { name: 'Rename' }));
    expect(
      await screen.findByText(/Name must start with a letter or underscore/),
    ).toBeInTheDocument();
    expect(changeTableName).not.toHaveBeenCalled();
  });

  it('renames with the introspected table type and navigates to the renamed table', async () => {
    const { onClose } = setup();
    const input = await getNameInput();
    await userEvent.clear(input);
    await userEvent.type(input, 'purchases');
    await userEvent.click(screen.getByRole('button', { name: 'Rename' }));

    await waitFor(() =>
      expect(changeTableName).toHaveBeenCalledWith({
        source: { name: 'default', kind: 'postgres' },
        table: { table, type: 'VIEW' },
        newName: 'purchases',
      }),
    );

    act(() => onSuccess?.(true, { newName: 'purchases' }));

    await waitFor(() => expect(onClose).toHaveBeenCalled());
    expect(screen.getByTestId('location')).toHaveTextContent(
      new RegExp(
        encodeURIComponent(
          JSON.stringify({ name: 'purchases', schema: 'public' }),
        ),
      ),
    );
    expect(screen.getByTestId('location')).toHaveTextContent(/\/modify/);
  });
});
