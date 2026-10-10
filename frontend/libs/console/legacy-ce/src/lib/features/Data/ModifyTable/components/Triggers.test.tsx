import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { Theme } from '@radix-ui/themes';
import type { MetadataTable, Source } from '@hasura/shared/types';
import { Triggers } from './Triggers';
import { getEmptyTriggerFormValues, TriggerFormValues } from './TriggerForm';

const dropTrigger = vi.fn().mockResolvedValue(true);
const createTrigger = vi.fn();
const confirmSpy = vi.fn(
  ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => onConfirm(),
);
let dropSupported = true;
let createSupported = true;
let existing: {
  name: string;
  timing?: string;
  events?: string;
  definition?: string;
  createStatement?: string;
}[] = [];
let submittedValues: TriggerFormValues;

vi.mock('@hasura/metadata/data-source', () => ({
  useDropTrigger: () => ({ mutateAsync: dropTrigger }),
  useCreateTrigger: () => ({
    mutate: createTrigger,
    isPending: false,
    error: null,
  }),
  useTableTriggers: () => ({ data: existing, isLoading: false }),
  useTriggerFunctions: () => ({ data: [] }),
  getDatabaseMethods: () => ({
    introspection: { getTriggerFunctions: () => undefined },
    modify: {
      ...(dropSupported ? { dropTrigger: () => undefined } : {}),
      ...(createSupported ? { createTrigger: () => undefined } : {}),
    },
  }),
}));

vi.mock('@hasura/shared/ui', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return { ...actual, useDestructiveConfirm: () => confirmSpy };
});

// Stub the dialog so onSubmit can be driven deterministically; the form itself
// is covered in TriggerForm.test.tsx.
vi.mock('./TriggerForm', async (importOriginal) => {
  const actual = (await importOriginal()) as Record<string, unknown>;
  return {
    ...actual,
    TriggerForm: (props: { onSubmit: (v: TriggerFormValues) => void }) => (
      <div data-testid="trigger-form">
        <button onClick={() => props.onSubmit(submittedValues)}>submit</button>
      </div>
    ),
  };
});

const source = { name: 'default', kind: 'postgres' } as Source;
const table = { table: { name: 'orders', schema: 'public' } } as MetadataTable;

const setup = () =>
  render(
    <Theme>
      <Triggers source={source} table={table} isView={false} />
    </Theme>,
  );

describe('ModifyTable Triggers', () => {
  beforeEach(() => {
    dropTrigger.mockClear();
    createTrigger.mockClear();
    confirmSpy.mockClear();
    dropSupported = true;
    createSupported = true;
    existing = [{ name: 'set_updated_at', timing: 'BEFORE', events: 'UPDATE' }];
    submittedValues = {
      ...getEmptyTriggerFormValues('public'),
      name: 'orders_audit',
      timing: 'AFTER',
      events: ['INSERT', 'DELETE'],
      newFunctionName: 'orders_audit_fn',
    };
  });

  it('lists existing triggers', () => {
    setup();
    expect(screen.getByText('set_updated_at')).toBeInTheDocument();
  });

  it('shows the trigger definition when expanded', async () => {
    existing = [
      {
        name: 'set_updated_at',
        timing: 'BEFORE',
        events: 'UPDATE',
        definition: 'EXECUTE FUNCTION set_current_timestamp()',
        createStatement:
          'CREATE TRIGGER set_updated_at BEFORE UPDATE ON orders FOR EACH ROW EXECUTE FUNCTION set_current_timestamp()',
      },
    ];
    setup();
    // The highlighted SQL is split across spans, so match on text content.
    expect(document.body).not.toHaveTextContent(/FOR EACH ROW/);
    await userEvent.click(screen.getByText('set_updated_at'));
    await waitFor(() =>
      expect(document.body).toHaveTextContent(
        /CREATE TRIGGER\s+set_updated_at[\s\S]*FOR EACH ROW/,
      ),
    );
  });

  it('removes a trigger via confirmation', async () => {
    setup();
    await userEvent.click(
      screen.getByRole('button', { name: 'Remove trigger set_updated_at' }),
    );
    expect(confirmSpy).toHaveBeenCalledTimes(1);
    await waitFor(() =>
      expect(dropTrigger).toHaveBeenCalledWith(
        expect.objectContaining({
          triggerName: 'set_updated_at',
          table: table.table,
        }),
      ),
    );
  });

  it('hides remove when the driver lacks dropTrigger', () => {
    dropSupported = false;
    setup();
    expect(
      screen.queryByRole('button', { name: /remove/i }),
    ).not.toBeInTheDocument();
  });

  it('does not offer to remove triggers generated for Hasura event triggers', () => {
    existing = [
      {
        name: 'notify_hasura_orders_insert_INSERT',
        timing: 'AFTER',
        events: 'INSERT',
        definition:
          'EXECUTE FUNCTION hdb_catalog."notify_hasura_orders_insert_INSERT"()',
      },
    ];
    setup();
    expect(screen.getByText('event trigger')).toBeInTheDocument();
    expect(
      screen.queryByRole('button', { name: /remove/i }),
    ).not.toBeInTheDocument();
  });

  it('creates a trigger with a new function', async () => {
    setup();
    await userEvent.click(screen.getByRole('button', { name: /add trigger/i }));
    await userEvent.click(screen.getByRole('button', { name: 'submit' }));
    expect(createTrigger).toHaveBeenCalledWith({
      source: { name: 'default', kind: 'postgres' },
      table: table.table,
      triggerName: 'orders_audit',
      timing: 'AFTER',
      events: ['INSERT', 'DELETE'],
      forEach: 'ROW',
      condition: undefined,
      function: { schema: 'public', name: 'orders_audit_fn' },
      newFunctionBody: submittedValues.newFunctionBody,
    });
  });

  it('creates a trigger on an existing function', async () => {
    submittedValues = {
      ...submittedValues,
      functionMode: 'existing',
      existingFunction: { schema: 'audit', name: 'log_change' },
      condition: 'NEW.total > 0',
    };
    setup();
    await userEvent.click(screen.getByRole('button', { name: /add trigger/i }));
    await userEvent.click(screen.getByRole('button', { name: 'submit' }));
    expect(createTrigger).toHaveBeenCalledWith(
      expect.objectContaining({
        function: { schema: 'audit', name: 'log_change' },
        condition: 'NEW.total > 0',
        newFunctionBody: undefined,
      }),
    );
  });

  it('hides the add button when the driver lacks createTrigger', () => {
    createSupported = false;
    setup();
    expect(
      screen.queryByRole('button', { name: /add trigger/i }),
    ).not.toBeInTheDocument();
  });
});
