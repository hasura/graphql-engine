import { render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { SelectTable } from './SelectTable';
import { tableContext } from './TableProvider';
import { rowPermissionsContext } from './RowPermissionsProvider';
import { rootTableContext } from './RootTableProvider';
import { Tables } from './types';

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

// Regression for the exhaustive-deps fix (adding `table`). The sync effect must
// push the selected table into the table context exactly once for a stable
// value, only when the comparator is `_table`, and must not loop.

const setTable = vi.fn();
const setComparator = vi.fn();

const tables: Tables = [
  {
    table: ['Album'],
    columns: [],
    relationships: [],
    dataSource: { name: 'pg', kind: 'postgres' },
    computedFields: [],
  },
  {
    table: ['Track'],
    columns: [],
    relationships: [],
    dataSource: { name: 'pg', kind: 'postgres' },
    computedFields: [],
  },
] as unknown as Tables;

const renderSelect = (path: string[], value: any) =>
  render(
    <Theme>
      <rootTableContext.Provider
        value={{ table: ['Album'], tables, rootTable: undefined }}
      >
        <rowPermissionsContext.Provider
          value={
            {
              comparators: {},
              operators: {},
              permissions: {},
              setValue: () => undefined,
              setKey: () => undefined,
              setPermissions: () => undefined,
            } as any
          }
        >
          <tableContext.Provider
            value={
              {
                table: [],
                setTable,
                setComparator,
                columns: [],
                computedFields: [],
                relationships: [],
                setColumns: () => undefined,
                setComputedFields: () => undefined,
                setRelationships: () => undefined,
                comparator: undefined,
              } as any
            }
          >
            <SelectTable
              componentLevelId="select-table"
              path={path}
              value={value}
            />
          </tableContext.Provider>
        </rowPermissionsContext.Provider>
      </rootTableContext.Provider>
    </Theme>,
  );

describe('SelectTable', () => {
  beforeEach(() => {
    setTable.mockClear();
    setComparator.mockClear();
  });

  it('renders the table select', () => {
    renderSelect(['x', '_table'], ['Album']);
    expect(screen.getByRole('combobox')).toBeInTheDocument();
  });

  it('syncs the selected table into context once for a _table comparator', () => {
    renderSelect(['x', '_table'], ['Album']);
    expect(setTable).toHaveBeenCalledTimes(1);
    expect(setTable).toHaveBeenCalledWith(['Album']);
  });

  it('does not sync when the comparator is not _table', () => {
    renderSelect(['x', '_eq'], ['Album']);
    expect(setTable).not.toHaveBeenCalled();
  });
});
