// @vitest-environment jsdom
import { renderHook } from '@testing-library/react';
import { vi } from 'vitest';
import { useOnClickOutside } from './useOnClickOutside';

const fireMouseDown = (target: EventTarget) => {
  const event = new MouseEvent('mousedown', { bubbles: true });
  target.dispatchEvent(event);
};

describe('useOnClickOutside', () => {
  it('calls the handler when clicking outside all refs', () => {
    const inside = document.createElement('div');
    const outside = document.createElement('div');
    document.body.append(inside, outside);

    const ref = { current: inside };
    const handler = vi.fn();

    renderHook(() => useOnClickOutside([ref], handler));

    fireMouseDown(outside);

    expect(handler).toHaveBeenCalledTimes(1);

    inside.remove();
    outside.remove();
  });

  it('does not call the handler when clicking inside a ref', () => {
    const inside = document.createElement('div');
    const child = document.createElement('span');
    inside.appendChild(child);
    document.body.append(inside);

    const ref = { current: inside };
    const handler = vi.fn();

    renderHook(() => useOnClickOutside([ref], handler));

    fireMouseDown(child);

    expect(handler).not.toHaveBeenCalled();

    inside.remove();
  });

  it('does not call the handler when a click is outside a mounted ref but another ref in the list is not yet mounted (null)', () => {
    // Regression test: previously, any ref whose `current` was null was
    // treated as "clicked inside", so the handler could never fire as long
    // as at least one ref in the list hadn't mounted yet.
    const mounted = document.createElement('div');
    const outside = document.createElement('div');
    document.body.append(mounted, outside);

    const mountedRef = { current: mounted };
    const unmountedRef = { current: null as HTMLElement | null };
    const handler = vi.fn();

    renderHook(() => useOnClickOutside([mountedRef, unmountedRef], handler));

    fireMouseDown(outside);

    expect(handler).toHaveBeenCalledTimes(1);

    mounted.remove();
    outside.remove();
  });

  it('removes its event listeners on unmount', () => {
    const outside = document.createElement('div');
    document.body.append(outside);

    const ref = { current: null as HTMLElement | null };
    const handler = vi.fn();

    const { unmount } = renderHook(() => useOnClickOutside([ref], handler));
    unmount();

    fireMouseDown(outside);

    expect(handler).not.toHaveBeenCalled();

    outside.remove();
  });
});
