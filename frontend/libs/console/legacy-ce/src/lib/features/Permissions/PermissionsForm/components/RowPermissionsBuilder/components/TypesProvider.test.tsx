import { useContext } from 'react';
import { render, screen } from '@testing-library/react';
import { TypesProvider, typesContext } from './TypesProvider';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { rootTableContext } from './RootTableProvider';

// Regression for the set-state-in-effect -> "adjust state while rendering" fix:
// `types` must be (re)computed from permissions/table/tables without an effect,
// update when permissions change, and not re-render in a loop.

const getPermissionTypesMock = vi.fn(
  (_tables: any, _table: any, permissions: any) => ({
    computed: { type: 'column' as const },
    marker: { type: permissions?.marker ?? ('value' as const) },
  }),
);

vi.mock('./utils/typeProviderHelpers', () => ({
  getPermissionTypes: (tables: any, table: any, permissions: any) =>
    getPermissionTypesMock(tables, table, permissions),
}));

const renderSpy = vi.fn();

const Consumer = () => {
  const { types } = useContext(typesContext);
  renderSpy();
  return <div data-testid="types">{JSON.stringify(types)}</div>;
};

const wrapper = (permissions: Record<string, any>) => (
  <rootTableContext.Provider
    value={{ table: ['Album'], tables: [], rootTable: undefined }}
  >
    <rowPermissionsContext.Provider
      value={
        {
          comparators: {},
          operators: {},
          permissions,
          setValue: () => undefined,
          setKey: () => undefined,
          setPermissions: () => undefined,
        } as any
      }
    >
      <TypesProvider>
        <Consumer />
      </TypesProvider>
    </rowPermissionsContext.Provider>
  </rootTableContext.Provider>
);

describe('TypesProvider', () => {
  beforeEach(() => {
    renderSpy.mockClear();
    getPermissionTypesMock.mockClear();
  });

  it('computes types on the initial render', () => {
    render(wrapper({ marker: 'column' }));
    expect(screen.getByTestId('types')).toHaveTextContent(
      JSON.stringify({
        computed: { type: 'column' },
        marker: { type: 'column' },
      }),
    );
  });

  it('recomputes types when permissions change', () => {
    const { rerender } = render(wrapper({ marker: 'column' }));
    rerender(wrapper({ marker: 'relationship' }));
    expect(screen.getByTestId('types')).toHaveTextContent(
      JSON.stringify({
        computed: { type: 'column' },
        marker: { type: 'relationship' },
      }),
    );
  });

  it('does not recompute or re-render in a loop for stable inputs', () => {
    render(wrapper({ marker: 'column' }));
    // getPermissionTypes runs on mount; the sync must not loop.
    expect(getPermissionTypesMock.mock.calls.length).toBeLessThanOrEqual(2);
    expect(renderSpy.mock.calls.length).toBeLessThanOrEqual(2);
  });
});
