import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { getEmptyTriggerFormValues, TriggerForm } from './TriggerForm';

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

const findInput = (name: string) =>
  waitFor(() => {
    const el = document.querySelector<HTMLInputElement>(
      `input[name="${name}"]`,
    );
    if (!el) throw new Error(`input "${name}" not rendered`);
    return el;
  });

const setup = (overrides = {}) => {
  const onSubmit = vi.fn();
  render(
    <TriggerForm
      defaultValues={{ ...getEmptyTriggerFormValues('public'), ...overrides }}
      functions={[]}
      onSubmit={onSubmit}
      onClose={vi.fn()}
    />,
  );
  return { onSubmit };
};

// The dialog mounts its form after a short delay; a click on the footer before
// that is dropped, so wait for the form first.
const submit = async () => {
  await findInput('name');
  await userEvent.click(screen.getByRole('button', { name: 'Add Trigger' }));
};

describe('TriggerForm', () => {
  it('submits a trigger with a new function', async () => {
    const { onSubmit } = setup();
    await userEvent.type(await findInput('name'), 'orders_audit');
    await userEvent.type(await findInput('newFunctionName'), 'orders_audit_fn');
    await submit();

    await waitFor(() => expect(onSubmit).toHaveBeenCalled());
    expect(onSubmit.mock.calls[0][0]).toMatchObject({
      name: 'orders_audit',
      timing: 'BEFORE',
      events: ['INSERT'],
      forEach: 'ROW',
      functionMode: 'new',
      newFunctionSchema: 'public',
      newFunctionName: 'orders_audit_fn',
    });
  });

  it('requires TRUNCATE triggers to be FOR EACH STATEMENT', async () => {
    const { onSubmit } = setup({
      name: 't',
      newFunctionName: 'fn',
      events: ['TRUNCATE'],
    });
    await submit();
    expect(
      await screen.findByText('TRUNCATE triggers must be FOR EACH STATEMENT'),
    ).toBeInTheDocument();
    expect(onSubmit).not.toHaveBeenCalled();
  });

  it('requires a function when using an existing one', async () => {
    const { onSubmit } = setup({ name: 't', functionMode: 'existing' });
    await submit();
    expect(
      await screen.findByText('Select a trigger function'),
    ).toBeInTheDocument();
    expect(onSubmit).not.toHaveBeenCalled();
  });
});
