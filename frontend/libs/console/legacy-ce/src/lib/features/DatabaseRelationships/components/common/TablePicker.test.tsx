import { render, screen, waitFor } from '@testing-library/react';
import { FormProvider, useForm, useWatch } from 'react-hook-form';
import { TablePicker } from './TablePicker';

// MOCKS (labelled): the data layer and mapping utils are stubbed so the test can
// drive exactly what `defaultValue` (and therefore its `.type`) resolves to.
let currentDefault: unknown = {
  type: 'table',
  value: { dataSourceName: 'pg', table: 'users' },
};

const mapItemsSpy = vi.fn((items: unknown[]) => [{ mapped: items[0] }]);

vi.mock('@hasura/metadata/api', () => ({
  // non-empty data -> the component renders the picker, not the skeleton
  useMetadata: () => ({ data: [{ name: 'pg' }] }),
}));

vi.mock('@hasura/metadata/helpers', () => ({
  MetadataSelectors: { getSources: () => () => [] },
}));

vi.mock('./TablePicker.utils', () => ({
  mapMetadataSourceToSelectorItems: () => [{ type: 'table', value: {} }],
  getDefaultSourceSelectorItem: () => currentDefault,
}));

vi.mock('./SourcePicker/SourcePicker.utils', () => ({
  mapItemsToSourceOptions: (items: unknown[]) => mapItemsSpy(items),
}));

vi.mock('./SourcePicker/SourcePicker', () => ({
  SourcePicker: () => <div data-testid="source-picker" />,
}));

// Renders the current `fromSource` form value so assertions can read what the
// TablePicker effect applied, without reaching into the form instance.
const ValueProbe = () => {
  const value = useWatch({ name: 'fromSource' });
  return <div data-testid="form-value">{JSON.stringify(value ?? null)}</div>;
};

const Harness = ({ disabled = false }: { disabled?: boolean }) => {
  const methods = useForm<Record<string, unknown>>({ defaultValues: {} });
  return (
    <FormProvider {...methods}>
      <TablePicker type="fromSource" isCurrentSource disabled={disabled} />
      <ValueProbe />
    </FormProvider>
  );
};

const formValue = () =>
  JSON.parse(screen.getByTestId('form-value').textContent || 'null');

beforeEach(() => {
  mapItemsSpy.mockClear();
  currentDefault = {
    type: 'table',
    value: { dataSourceName: 'pg', table: 'users' },
  };
});

describe('TablePicker (default value sync effect)', () => {
  it('applies the resolved default to the form on mount', async () => {
    render(<Harness />);
    await waitFor(() => expect(mapItemsSpy).toHaveBeenCalled());
    expect(formValue()).toEqual({ mapped: currentDefault });
  });

  it('re-applies the LATEST default when the resolved type changes (latest-ref)', async () => {
    const { rerender } = render(<Harness />);
    await waitFor(() =>
      expect(formValue()).toEqual({ mapped: currentDefault }),
    );

    const newDefault = {
      type: 'remoteSchema',
      value: { remoteSchemaName: 'rs' },
    };
    currentDefault = newDefault;
    // Force a re-render; the resolved default's `.type` flips table ->
    // remoteSchema, which must re-run the effect with the new value.
    rerender(<Harness disabled />);

    await waitFor(() => expect(formValue()).toEqual({ mapped: newDefault }));
  });

  it('does not loop while the resolved type stays the same', async () => {
    render(<Harness />);
    await waitFor(() => expect(mapItemsSpy).toHaveBeenCalled());

    // Give any cascading render a chance to fire; applying the value changes the
    // watched form fields but NOT the resolved `.type`, so the effect must not
    // keep firing.
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(mapItemsSpy.mock.calls.length).toBeLessThan(3);
  });
});
