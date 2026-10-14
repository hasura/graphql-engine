import { fireEvent, render, screen } from '@testing-library/react';
import { ErrorBox } from './ErrorBox';
import { OneClickDeploymentState } from '../../../types';

const renderErrorBox = (
  logId: string,
  retryAction = vi.fn(),
  error: Record<string, any> = { error: { message: 'something went wrong' } },
) => {
  const utils = render(
    <ErrorBox
      step={OneClickDeploymentState.CloningGitRepository}
      error={error}
      logId={logId}
      retryAction={retryAction}
      fallbackApps={[]}
    />,
  );
  return { retryAction, ...utils };
};

describe('ErrorBox', () => {
  it('renders the error message and an enabled retry button initially', () => {
    renderErrorBox('log-1');
    const button = screen.getByTestId('one-click-deployment-error-retry');
    expect(button).toBeInTheDocument();
    expect(button).toBeEnabled();
  });

  it('marks the button as retrying after a retry click and calls retryAction once', () => {
    const { retryAction } = renderErrorBox('log-1');
    const button = screen.getByTestId('one-click-deployment-error-retry');

    fireEvent.click(button);
    expect(retryAction).toHaveBeenCalledTimes(1);
    expect(button).toBeDisabled();

    // further clicks while retrying should not re-trigger the action
    fireEvent.click(button);
    expect(retryAction).toHaveBeenCalledTimes(1);
  });

  it('resets the retrying state when logId changes (new log arrives)', () => {
    const retryAction = vi.fn();
    const { rerender } = render(
      <ErrorBox
        step={OneClickDeploymentState.CloningGitRepository}
        error={{ error: { message: 'boom' } }}
        logId="log-1"
        retryAction={retryAction}
        fallbackApps={[]}
      />,
    );

    fireEvent.click(screen.getByTestId('one-click-deployment-error-retry'));
    expect(
      screen.getByTestId('one-click-deployment-error-retry'),
    ).toBeDisabled();

    // a new logId means the retry completed -> button becomes clickable again
    rerender(
      <ErrorBox
        step={OneClickDeploymentState.CloningGitRepository}
        error={{ error: { message: 'boom' } }}
        logId="log-2"
        retryAction={retryAction}
        fallbackApps={[]}
      />,
    );
    expect(
      screen.getByTestId('one-click-deployment-error-retry'),
    ).toBeEnabled();

    // retry works again with the new log
    fireEvent.click(screen.getByTestId('one-click-deployment-error-retry'));
    expect(retryAction).toHaveBeenCalledTimes(2);
  });

  it('does not reset retrying state on re-render with the same logId', () => {
    const retryAction = vi.fn();
    const { rerender } = render(
      <ErrorBox
        step={OneClickDeploymentState.CloningGitRepository}
        error={{ error: { message: 'boom' } }}
        logId="log-1"
        retryAction={retryAction}
        fallbackApps={[]}
      />,
    );
    fireEvent.click(screen.getByTestId('one-click-deployment-error-retry'));
    expect(
      screen.getByTestId('one-click-deployment-error-retry'),
    ).toBeDisabled();

    rerender(
      <ErrorBox
        step={OneClickDeploymentState.CloningGitRepository}
        error={{ error: { message: 'boom' } }}
        logId="log-1"
        retryAction={retryAction}
        fallbackApps={[]}
      />,
    );
    // still retrying, since the log did not change
    expect(
      screen.getByTestId('one-click-deployment-error-retry'),
    ).toBeDisabled();
  });
});
