import * as React from 'react';
import {
  act,
  fireEvent,
  screen,
  cleanup,
  render,
} from '@testing-library/react';
import { RedirectCountDown } from './RedirectCountDown';

describe('RedirectCountdown', () => {
  beforeEach(() => {
    jest.clearAllMocks();
    jest.useFakeTimers();
    cleanup();
  });

  it('when timer runs out, triggers redirect correctly', () => {
    const redirect = jest.fn();
    render(<RedirectCountDown redirect={redirect} timeSeconds={2} />);
    expect(screen.getByTestId('redirect-countdown')).toBeInTheDocument();
    expect(redirect).not.toHaveBeenCalled();
    act(() => {
      jest.advanceTimersByTime(4000);
    });
    expect(redirect).toHaveBeenCalled();
  });

  it('when button clicked, triggers redirect', () => {
    const redirect = jest.fn();
    render(<RedirectCountDown redirect={redirect} timeSeconds={10} />);
    expect(screen.getByTestId('redirect-countdown')).toBeInTheDocument();
    expect(redirect).not.toHaveBeenCalled();
    act(() => {
      fireEvent.click(screen.getByTestId('redirect-countdown-redirect-button'));
    });
    expect(redirect).toHaveBeenCalled();
  });
});
