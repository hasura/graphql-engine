import { act, configure, render, screen } from '@testing-library/react';
import { FormProvider, useForm } from 'react-hook-form';
import { AppTheme } from '@hasura/shared/ui';
import { MapSelector, TypeMap } from './MapSelector';
import { Schema } from '../schema';

// jsdom does not implement ResizeObserver, which Radix Select relies on.
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

// MapSelector reads/writes its `mapping` field through `useFormContext<Schema>()`
// (hardcoded field name "mapping"), so it must be rendered inside a
// react-hook-form `FormProvider`. It uses `ReactSelect` (an async combobox,
// not a native `<select>`) for the field/column pickers, so interactions
// with those are exercised indirectly through form state rather than native
// `<select>` events. The delete icon exposes a `data-test` attribute we can
// query directly.
const renderMapSelector = (mapping: TypeMap[]) => {
  let formMethods!: ReturnType<typeof useForm<Pick<Schema, 'mapping'>>>;

  const Wrapper = () => {
    formMethods = useForm<Pick<Schema, 'mapping'>>({
      defaultValues: { mapping },
    });

    return (
      <AppTheme>
        <FormProvider {...formMethods}>
          <MapSelector
            types={['id', 'name', 'email']}
            columns={['id', 'full_name', 'email_address']}
            mapping={mapping}
            setMapping={() => {}}
          />
        </FormProvider>
      </AppTheme>
    );
  };

  const view = render(<Wrapper />);

  return {
    ...view,
    getMapping: () => formMethods.getValues('mapping'),
    // The remove control is identified by a `data-test` attribute; query it via
    // screen (testIdAttribute is set to `data-test` for this suite) rather than
    // reaching into the render container.
    removeIcon: (index: number) =>
      screen.queryByTestId(`remove-type-map-${index}`),
  };
};

describe('MapSelector', () => {
  // The remove control exposes a `data-test` hook rather than a `data-testid`.
  beforeAll(() => configure({ testIdAttribute: 'data-test' }));
  afterAll(() => configure({ testIdAttribute: 'data-testid' }));

  it('renders one row per existing type mapping, plus the relationship type select and an "add new" row', () => {
    renderMapSelector([
      { field: 'id', column: 'id' },
      { field: 'name', column: 'full_name' },
    ]);

    // relationship type selector is always rendered
    expect(screen.getByText(/select a relationship type/i)).toBeInTheDocument();

    // one row per existing mapping is rendered, showing both sides of the mapping
    expect(screen.getByText('name')).toBeVisible();
    expect(screen.getByText('full_name')).toBeVisible();

    // two "id" labels: the field side of the first row and the column side of the
    // first row both read "id"
    expect(screen.getAllByText('id')).toHaveLength(2);

    // column headers
    expect(screen.getByText('Source Field')).toBeVisible();
    expect(screen.getByText('Reference Column')).toBeVisible();
  });

  it('calls setValue with an updated array (not an array-like object) when a row is deleted', () => {
    const { getMapping, removeIcon } = renderMapSelector([
      { field: 'id', column: 'id' },
      { field: 'name', column: 'full_name' },
    ]);

    act(() => {
      removeIcon(1)!.dispatchEvent(new MouseEvent('click', { bubbles: true }));
    });

    const updatedMaps = getMapping();

    // Regression: onDeleteItem must produce a real array (not an
    // object-with-numeric-keys) so downstream consumers can safely spread it.
    expect(Array.isArray(updatedMaps)).toBe(true);
    expect(updatedMaps).toEqual([{ field: 'id', column: 'id' }]);
  });

  it('renders an empty state with just the "add new" row', () => {
    renderMapSelector([]);

    expect(screen.queryAllByText('Source Field')).toHaveLength(1);
    expect(screen.queryByTestId(/remove-type-map-0/)).not.toBeInTheDocument();
  });
});
