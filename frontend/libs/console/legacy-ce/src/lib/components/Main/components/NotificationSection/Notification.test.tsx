import { describe, expect, it, vi } from 'vitest';
import { Profiler } from 'react';
import { render, screen, fireEvent } from '@testing-library/react';

// The @hasura/shared/ui barrel pulls LoadingScreen -> lottie-react -> lottie-web,
// which touches a 2D canvas context that jsdom does not implement.
vi.mock('lottie-react', () => ({
  __esModule: true,
  default: () => null,
  LottieSvg: () => null,
}));

// SCSS modules are stubbed to an empty string by the vite config, which makes
// `styles.<class>` undefined. Echo the key names back so we can assert on the
// read/unread class toggling that encodes the component's local state.
vi.mock('../../Main.module.scss', () => ({
  __esModule: true,
  default: new Proxy({}, { get: (_target, key: string) => key }),
}));

import Notification from './Notification';

const baseProps = {
  subject: 'A subject',
  content: 'Some content',
  type: 'security' as any,
  created_at: Date.now(),
  start_date: null,
  priority: 1,
  expiry_date: null,
  id: 42,
  consoleScope: 'OSS' as any,
  scope: ['OSS'] as any,
};

const container = () =>
  document.querySelector('.updateBox') as HTMLElement | null;

describe('Notification read-state syncing', () => {
  it('renders as unread on initial render when is_read is false', () => {
    render(<Notification {...baseProps} is_read={false} />);
    expect(screen.getByText('A subject')).toBeInTheDocument();
    expect(container()?.className).toContain('unread');
    expect(container()?.className).not.toContain('read ');
    // the unread dot is visible (not hidden) while unread
    expect(document.querySelector('.unreadDot')?.className).not.toContain(
      'hideDot',
    );
  });

  it('syncs local read-state when the is_read prop changes', () => {
    const { rerender } = render(
      <Notification {...baseProps} is_read={false} />,
    );
    expect(container()?.className).toContain('unread');

    rerender(<Notification {...baseProps} is_read />);
    // during-render adjustment keeps the derived state in sync with the prop
    expect(container()?.className).toContain('read');
    expect(document.querySelector('.unreadDot')?.className).toContain(
      'hideDot',
    );

    // and back to unread when the prop flips again
    rerender(<Notification {...baseProps} is_read={false} />);
    expect(container()?.className).toContain('unread');
    expect(document.querySelector('.unreadDot')?.className).not.toContain(
      'hideDot',
    );
  });

  it('marks itself read locally on click and forwards the id', () => {
    const onClick = vi.fn();
    render(<Notification {...baseProps} is_read={false} onClick={onClick} />);
    expect(container()?.className).toContain('unread');

    fireEvent.click(container() as HTMLElement);

    expect(onClick).toHaveBeenCalledTimes(1);
    expect(onClick).toHaveBeenCalledWith(42);
    expect(container()?.className).toContain('read');
  });

  it('does not re-render in a runaway loop when the prop changes', () => {
    let commitCount = 0;
    const onRender = () => {
      commitCount += 1;
    };
    const { rerender } = render(
      <Profiler id="notification" onRender={onRender}>
        <Notification {...baseProps} is_read={false} />
      </Profiler>,
    );
    const afterMount = commitCount;

    rerender(
      <Profiler id="notification" onRender={onRender}>
        <Notification {...baseProps} is_read />
      </Profiler>,
    );

    // A single prop change settles in a bounded number of commits (the extra
    // render from the in-render state adjustment is collapsed by React).
    expect(commitCount - afterMount).toBeLessThanOrEqual(2);
  });
});
