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
    act(() => {
      fireEvent.click(screen.getByTestId('redirect-countdown-redirect-button'));
    });
    expect(redirect).toHaveBeenCalled();
  });
});
