import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { FormProvider, useForm } from 'react-hook-form';
import { ConsentCheckbox } from './ConsentCheckbox';

const FIELD = 'consent';
const ERROR_TEXT = 'You must agree to continue';

// MOCKS (labelled): stub the UI primitives. The Checkbox stub also counts how
// many times ConsentCheckbox renders (it renders on every render) so we can
// assert the derived error message does not cause a render loop.
const renderStats = { count: 0 };
vi.mock('@hasura/shared/ui', () => ({
  Checkbox: ({ children }: { children: React.ReactNode }) => {
    renderStats.count += 1;
    return <div data-testid="checkbox">{children}</div>;
  },
  ErrorMessage: ({ error }: { error: string }) => <div>{error}</div>,
}));

const Harness = () => {
  const methods = useForm<{ consent: boolean }>({
    defaultValues: { consent: false },
  });
  return (
    <FormProvider {...methods}>
      <ConsentCheckbox fieldName={FIELD} />
      <button
        type="button"
        onClick={() => methods.setError(FIELD, { message: ERROR_TEXT })}
      >
        trigger-error
      </button>
      <button type="button" onClick={() => methods.setValue(FIELD, true)}>
        accept
      </button>
    </FormProvider>
  );
};

beforeEach(() => {
  renderStats.count = 0;
});

describe('ConsentCheckbox (derived error message)', () => {
  it('renders the consent copy with no error initially', () => {
    render(<Harness />);
    expect(screen.getByText(/Terms of Service/i)).toBeInTheDocument();
    expect(screen.queryByText(ERROR_TEXT)).not.toBeInTheDocument();
  });

  it('shows the form error when the field is unchecked and clears it once accepted', async () => {
    const user = userEvent.setup();
    render(<Harness />);

    await user.click(screen.getByRole('button', { name: 'trigger-error' }));
    expect(await screen.findByText(ERROR_TEXT)).toBeInTheDocument();

    // Accepting makes the field truthy -> the derived message resolves to ''.
    await user.click(screen.getByRole('button', { name: 'accept' }));
    await waitFor(() =>
      expect(screen.queryByText(ERROR_TEXT)).not.toBeInTheDocument(),
    );
  });

  it('does not re-render in a loop while displaying the derived error', async () => {
    const user = userEvent.setup();
    render(<Harness />);

    await user.click(screen.getByRole('button', { name: 'trigger-error' }));
    expect(await screen.findByText(ERROR_TEXT)).toBeInTheDocument();

    // Allow any stray scheduled render to flush, then assert the render count is
    // bounded (a setState-in-effect loop would blow past this).
    await new Promise((resolve) => setTimeout(resolve, 50));
    expect(renderStats.count).toBeLessThan(6);
  });
});
