import '@testing-library/jest-dom/vitest';
import { act, render, screen } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import { vi } from 'vitest';
import { DelayedDialog } from './Dialog';

describe('DelayedDialog', () => {
  beforeEach(() => {
    vi.useFakeTimers();
  });

  afterEach(() => {
    vi.runOnlyPendingTimers();
    vi.useRealTimers();
  });

  it('renders its children only after the delay elapses', () => {
    render(
      <Theme>
        <DelayedDialog delay={100} title="Delayed">
          <div>delayed content</div>
        </DelayedDialog>
      </Theme>,
    );

    expect(screen.queryByText('delayed content')).not.toBeInTheDocument();

    act(() => {
      vi.advanceTimersByTime(50);
    });
    expect(screen.queryByText('delayed content')).not.toBeInTheDocument();

    act(() => {
      vi.advanceTimersByTime(50);
    });
    expect(screen.getByText('delayed content')).toBeInTheDocument();
  });

  it('shows the children once and does not re-mount them on further timers', () => {
    const renderSpy = vi.fn();
    const Child = () => {
      renderSpy();
      return <div>delayed content</div>;
    };

    render(
      <Theme>
        <DelayedDialog delay={100} title="Delayed">
          <Child />
        </DelayedDialog>
      </Theme>,
    );

    expect(renderSpy).not.toHaveBeenCalled();

    act(() => {
      vi.advanceTimersByTime(100);
    });

    expect(screen.getByText('delayed content')).toBeInTheDocument();
    const callsAfterShow = renderSpy.mock.calls.length;
    expect(callsAfterShow).toBeGreaterThan(0);

    // no additional timer should re-render/re-mount the children
    act(() => {
      vi.advanceTimersByTime(1000);
    });
    expect(renderSpy.mock.calls.length).toBe(callsAfterShow);
  });
});
