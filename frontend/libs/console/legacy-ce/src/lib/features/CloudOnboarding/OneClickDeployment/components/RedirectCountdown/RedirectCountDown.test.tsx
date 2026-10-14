import * as React from 'react';
import { act, fireEvent, screen, render } from '@testing-library/react';
import { RedirectCountDown } from './RedirectCountDown';

describe('RedirectCountdown', () => {
  beforeEach(() => {
    vi.clearAllMocks();
    vi.useFakeTimers();
  });

  it('when timer runs out, triggers redirect correctly', () => {
    const redirect = vi.fn();
    render(<RedirectCountDown redirect={redirect} timeSeconds={2} />);
    expect(screen.getByTestId('redirect-countdown')).toBeInTheDocument();
    expect(redirect).not.toHaveBeenCalled();
    act(() => {
      vi.advanceTimersByTime(4000);
    });
    expect(redirect).toHaveBeenCalled();
  });

  it('when button clicked, triggers redirect', () => {
    const redirect = vi.fn();
    render(<RedirectCountDown redirect={redirect} timeSeconds={10} />);
    expect(screen.getByTestId('redirect-countdown')).toBeInTheDocument();
    expect(redirect).not.toHaveBeenCalled();
    fireEvent.click(screen.getByTestId('redirect-countdown-redirect-button'));
    expect(redirect).toHaveBeenCalled();
  });

  it('uses the latest redirect prop when the timer fires after a re-render', () => {
    const firstRedirect = vi.fn();
    const secondRedirect = vi.fn();
    const { rerender } = render(
      <RedirectCountDown redirect={firstRedirect} timeSeconds={2} />,
    );

    // parent re-renders with a new redirect callback before the timer elapses
    rerender(<RedirectCountDown redirect={secondRedirect} timeSeconds={2} />);

    act(() => {
      vi.advanceTimersByTime(4000);
    });

    // the mount-only timer must call the LATEST redirect, never the stale one
    expect(firstRedirect).not.toHaveBeenCalled();
    expect(secondRedirect).toHaveBeenCalled();
  });
});
