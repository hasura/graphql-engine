import { render, screen, waitFor } from '@testing-library/react';
import userEvent from '@testing-library/user-event';
import { AppTheme } from '../../theme';
import { Button } from '../Button';
import {
  AlertProvider,
  useDestructiveConfirm,
  useDestructivePrompt,
  useHasuraAlert,
} from './AlertProvider';

// These tests lock in the shared confirmation primitives that every
// `getConfirmation` replacement relies on: a destructive confirm/prompt must
// not run its action on cancel or on a non-matching typed name, and a
// non-destructive confirm must only run its action when confirmed.

const renderWithProviders = (ui: React.ReactNode) =>
  render(
    <AppTheme>
      <AlertProvider>{ui}</AlertProvider>
    </AppTheme>,
  );

// Radix overlays set `pointer-events: none` on the body (AlertProvider works
// around this with a setTimeout); disable user-event's pointer-events check so
// clicks on the dialog buttons are not rejected.
const setupUser = () => userEvent.setup({ pointerEventsCheck: 0 });

describe('useDestructiveConfirm', () => {
  const Harness = ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => {
    const destructiveConfirm = useDestructiveConfirm();
    return (
      <Button
        onClick={() =>
          destructiveConfirm({
            resourceName: 'orders',
            resourceType: 'table',
            destroyTerm: 'delete',
            onConfirm,
          })
        }
      >
        open
      </Button>
    );
  };

  it('runs onConfirm when the user confirms', async () => {
    const user = setupUser();
    const onConfirm = vitest.fn().mockResolvedValue(true);
    renderWithProviders(<Harness onConfirm={onConfirm} />);

    await user.click(screen.getByText('open'));
    const confirmButton = await screen.findByTestId('alert-confirm-button');
    await user.click(confirmButton);

    await waitFor(() => expect(onConfirm).toHaveBeenCalledTimes(1));
  });

  it('does NOT run onConfirm when the user cancels', async () => {
    const user = setupUser();
    const onConfirm = vitest.fn().mockResolvedValue(true);
    renderWithProviders(<Harness onConfirm={onConfirm} />);

    await user.click(screen.getByText('open'));
    const cancelButton = await screen.findByText('Cancel');
    await user.click(cancelButton);

    // give any async handlers a chance to (not) fire
    await new Promise((r) => setTimeout(r, 0));
    expect(onConfirm).not.toHaveBeenCalled();
  });
});

describe('useDestructivePrompt (typed name confirmation)', () => {
  const Harness = ({ onConfirm }: { onConfirm: () => Promise<boolean> }) => {
    const destructivePrompt = useDestructivePrompt();
    return (
      <Button
        onClick={() =>
          destructivePrompt({
            resourceName: 'my_endpoint',
            resourceType: 'REST endpoint',
            destroyTerm: 'delete',
            onConfirm,
          })
        }
      >
        open
      </Button>
    );
  };

  it('runs onConfirm when the typed name matches', async () => {
    const user = setupUser();
    const onConfirm = vitest.fn().mockResolvedValue(true);
    renderWithProviders(<Harness onConfirm={onConfirm} />);

    await user.click(screen.getByText('open'));
    const input = await screen.findByRole('textbox');
    await user.type(input, 'my_endpoint');
    await user.click(screen.getByTestId('alert-confirm-button'));

    await waitFor(() => expect(onConfirm).toHaveBeenCalledTimes(1));
  });

  it('does NOT run onConfirm when the typed name does not match', async () => {
    const user = setupUser();
    const onConfirm = vitest.fn().mockResolvedValue(true);
    renderWithProviders(<Harness onConfirm={onConfirm} />);

    await user.click(screen.getByText('open'));
    const input = await screen.findByRole('textbox');
    await user.type(input, 'WRONG');
    await user.click(screen.getByTestId('alert-confirm-button'));

    // the mismatch toast is shown and the destructive action is skipped
    await waitFor(() =>
      expect(screen.getByText(/does not match/i)).toBeInTheDocument(),
    );
    expect(onConfirm).not.toHaveBeenCalled();
  });
});

describe('useHasuraAlert confirm (non-destructive)', () => {
  const Harness = ({
    onResult,
  }: {
    onResult: (confirmed: boolean) => void;
  }) => {
    const { hasuraConfirm } = useHasuraAlert();
    return (
      <Button
        onClick={() =>
          hasuraConfirm({
            title: 'Proceed?',
            message: 'A non-destructive confirmation',
            onClose: ({ confirmed }) => onResult(confirmed),
          })
        }
      >
        open
      </Button>
    );
  };

  it('reports confirmed=true when accepted', async () => {
    const user = setupUser();
    const onResult = vitest.fn();
    renderWithProviders(<Harness onResult={onResult} />);

    await user.click(screen.getByText('open'));
    await user.click(await screen.findByTestId('alert-confirm-button'));

    await waitFor(() => expect(onResult).toHaveBeenCalledWith(true));
  });

  it('reports confirmed=false when cancelled', async () => {
    const user = setupUser();
    const onResult = vitest.fn();
    renderWithProviders(<Harness onResult={onResult} />);

    await user.click(screen.getByText('open'));
    await user.click(await screen.findByText('Cancel'));

    await waitFor(() => expect(onResult).toHaveBeenCalledWith(false));
  });
});
