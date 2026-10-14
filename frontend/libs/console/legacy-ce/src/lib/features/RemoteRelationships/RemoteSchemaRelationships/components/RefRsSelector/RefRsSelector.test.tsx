import { render, waitFor } from '@testing-library/react';
import { FormProvider, useForm, UseFormReturn } from 'react-hook-form';
import { buildSchema } from 'graphql';
import { AppTheme } from '@hasura/shared/ui';

// RefRsSelector derives the list of operations from the introspected remote
// schema. Mock the data hook so the test can drive `data` / `isError` directly.
const { introspectSpy } = vi.hoisted(() => ({ introspectSpy: vi.fn() }));
vi.mock('@hasura/metadata/api', () => ({
  useIntrospectRemoteSchema: (...args: unknown[]) => introspectSpy(...args),
}));

import { RefRsSelector } from './RefRsSelector';

// jsdom does not implement ResizeObserver, which react-select relies on.
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

const querySchema = buildSchema(`
  type Query {
    userById: String
    users: String
  }
`);

const loaded = { data: querySchema, isLoading: false, isError: false };
const loading = { data: undefined, isLoading: true, isError: false };

type FormShape = {
  referenceRemoteSchema?: string;
  selectedOperation?: string;
  resultSet?: unknown;
};

const setup = (defaultValues: FormShape) => {
  let methods!: UseFormReturn<FormShape>;
  const Harness = () => {
    methods = useForm<FormShape>({ defaultValues });
    return (
      <AppTheme>
        <FormProvider {...methods}>
          <RefRsSelector allRemoteSchemas={['schema1', 'schema2']} />
        </FormProvider>
      </AppTheme>
    );
  };
  const view = render(<Harness />);
  return { ...view, getValues: () => methods.getValues() };
};

describe('RefRsSelector (operations derived from introspection)', () => {
  beforeEach(() => {
    introspectSpy.mockReset();
  });

  it('clears the selected operation when the loaded schema no longer offers it', async () => {
    introspectSpy.mockReturnValue(loaded);

    const { getValues } = setup({
      referenceRemoteSchema: 'schema1',
      selectedOperation: 'ghost',
    });

    await waitFor(() => expect(getValues().selectedOperation).toBe(''));
  });

  it('keeps a still-valid selected operation', async () => {
    introspectSpy.mockReturnValue(loaded);

    const { getValues } = setup({
      referenceRemoteSchema: 'schema1',
      selectedOperation: 'userById',
    });

    // Give the effects a chance to run, then assert the value was not cleared.
    await new Promise((resolve) => setTimeout(resolve, 100));
    expect(getValues().selectedOperation).toBe('userById');
  });

  it('does not clear or settle into a render loop while the schema is still loading', async () => {
    introspectSpy.mockReturnValue(loading);

    const { getValues } = setup({
      referenceRemoteSchema: 'schema1',
      selectedOperation: 'ghost',
    });

    // While loading, operations is empty (derived from undefined data) so the
    // clear guard does not fire: the user's selection is preserved.
    await new Promise((resolve) => setTimeout(resolve, 100));
    expect(getValues().selectedOperation).toBe('ghost');

    // No runaway: render-driven introspection calls stop once effects settle.
    const callsAfterSettle = introspectSpy.mock.calls.length;
    await new Promise((resolve) => setTimeout(resolve, 200));
    expect(introspectSpy.mock.calls.length).toBe(callsAfterSettle);
  });
});
