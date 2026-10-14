import { render, screen } from '@testing-library/react';
import { ColumnComparatorEntry } from './ColumnComparatorEntry';
import { rowPermissionsContext } from '../RowPermissionsProvider';

// Regression for the exhaustive-deps fix on the normalisation effect. The effect
// resets the value to [] only when it is NOT an array; for array values it must
// never fire, even across re-renders / value changes, and must not loop.

const setValue = vi.fn();

const renderEntry = (v: any, path: string[] = ['filter', 'col']) =>
  render(
    <rowPermissionsContext.Provider
      value={
        {
          comparators: {},
          operators: {},
          permissions: {},
          setValue,
          setKey: () => undefined,
          setPermissions: () => undefined,
        } as any
      }
    >
      <ColumnComparatorEntry k="_ceq" v={v} path={path} />
    </rowPermissionsContext.Provider>,
  );

describe('ColumnComparatorEntry', () => {
  beforeEach(() => {
    setValue.mockClear();
  });

  it('renders the column comparator select for an array value', () => {
    renderEntry(['AlbumId']);
    expect(
      screen.getByTestId('filter.col-column-comparator-entry'),
    ).toBeInTheDocument();
  });

  it('does not reset array values (no setValue) and does not loop', () => {
    renderEntry(['AlbumId']);
    expect(setValue).not.toHaveBeenCalled();
  });

  it('does not fire when the array value changes between renders', () => {
    const { rerender } = renderEntry(['AlbumId']);
    rerender(
      <rowPermissionsContext.Provider
        value={
          {
            comparators: {},
            operators: {},
            permissions: {},
            setValue,
            setKey: () => undefined,
            setPermissions: () => undefined,
          } as any
        }
      >
        <ColumnComparatorEntry
          k="_ceq"
          v={['$', '']}
          path={['filter', 'col']}
        />
      </rowPermissionsContext.Provider>,
    );
    expect(setValue).not.toHaveBeenCalled();
  });
});
