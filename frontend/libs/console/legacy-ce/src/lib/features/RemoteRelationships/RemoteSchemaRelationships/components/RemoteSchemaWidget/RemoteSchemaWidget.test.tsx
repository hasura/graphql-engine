import { render, waitFor } from '@testing-library/react';
import { FormProvider, useForm, UseFormReturn } from 'react-hook-form';
import { AppTheme } from '@hasura/shared/ui';

// Keep the introspection data undefined so the heavy RemoteSchemaTree child is
// not rendered; this test focuses on how the widget seeds its local
// relationshipFields state from the `serverRelationship` prop.
const { introspectSpy } = vi.hoisted(() => ({ introspectSpy: vi.fn() }));
vi.mock('@hasura/metadata/api', () => ({
  useIntrospectRemoteSchema: (...args: unknown[]) => introspectSpy(...args),
}));

import { RemoteSchemaWidget } from './RemoteSchemaWidget';
import {
  parseServerRelationship,
  buildServerRemoteFieldObject,
} from '../RemoteSchemaTree';
import { remote_rel_definition, customer_columns } from '../../__mocks__';

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

const seededResultSet = buildServerRemoteFieldObject(
  parseServerRelationship(remote_rel_definition as never),
);
const emptyResultSet = buildServerRemoteFieldObject([]);

type Props = React.ComponentProps<typeof RemoteSchemaWidget>;

const setup = (props: Partial<Props>) => {
  let methods!: UseFormReturn<any>;
  const Harness = ({ serverRelationship }: Partial<Props>) => {
    // `selectedOperation` is set so the widget's selectedOperation effect skips
    // its first run (the "modify relationship" flow), leaving the seeded fields
    // in place.
    methods = useForm<any>({
      defaultValues: { selectedOperation: 'someOperation', resultSet: '' },
    });
    return (
      <AppTheme>
        <FormProvider {...methods}>
          <RemoteSchemaWidget
            schemaName="remoteSchema1"
            fields={customer_columns}
            serverRelationship={serverRelationship}
          />
        </FormProvider>
      </AppTheme>
    );
  };
  const view = render(<Harness {...props} />);
  return {
    ...view,
    rerenderWith: (next: Partial<Props>) =>
      view.rerender(<Harness {...next} />),
    getResultSet: () => methods.getValues('resultSet'),
  };
};

describe('RemoteSchemaWidget (seeding from serverRelationship)', () => {
  beforeEach(() => {
    introspectSpy.mockReset();
    introspectSpy.mockReturnValue({
      data: undefined,
      isLoading: false,
      isError: false,
    });
  });

  it('seeds the relationship fields from an existing server relationship on mount', async () => {
    const { getResultSet } = setup({
      serverRelationship: remote_rel_definition as never,
    });

    await waitFor(() => expect(getResultSet()).toEqual(seededResultSet));
    // Sanity: the seeded result set is actually populated, not just the empty one.
    expect(getResultSet()).not.toEqual(emptyResultSet);
  });

  it('starts empty when there is no server relationship', async () => {
    const { getResultSet } = setup({ serverRelationship: undefined });

    await waitFor(() => expect(getResultSet()).toEqual(emptyResultSet));
  });

  it('re-seeds when the serverRelationship prop changes after mount, without a render loop', async () => {
    const { getResultSet, rerenderWith } = setup({
      serverRelationship: undefined,
    });

    await waitFor(() => expect(getResultSet()).toEqual(emptyResultSet));

    rerenderWith({ serverRelationship: remote_rel_definition as never });

    await waitFor(() => expect(getResultSet()).toEqual(seededResultSet));

    // No runaway: render-driven introspection calls stop once effects settle.
    const callsAfterSettle = introspectSpy.mock.calls.length;
    await new Promise((resolve) => setTimeout(resolve, 200));
    expect(introspectSpy.mock.calls.length).toBe(callsAfterSettle);
  });
});
