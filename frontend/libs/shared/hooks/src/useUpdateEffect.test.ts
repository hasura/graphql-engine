// @vitest-environment jsdom
import { renderHook } from '@testing-library/react';
import { vi } from 'vitest';
import { useUpdateEffect } from './useUpdateEffect';

describe('useUpdateEffect', () => {
  it('does not run the effect on the initial render', () => {
    const effect = vi.fn();

    renderHook(({ dep }) => useUpdateEffect(effect, [dep]), {
      initialProps: { dep: 0 },
    });

    expect(effect).not.toHaveBeenCalled();
  });

  it('runs the effect on subsequent renders', () => {
    const effect = vi.fn();

    const { rerender } = renderHook(
      ({ dep }) => useUpdateEffect(effect, [dep]),
      {
        initialProps: { dep: 0 },
      },
    );

    expect(effect).not.toHaveBeenCalled();

    rerender({ dep: 1 });
    expect(effect).toHaveBeenCalledTimes(1);

    rerender({ dep: 2 });
    expect(effect).toHaveBeenCalledTimes(2);
  });

  it('runs the effect cleanup between updates', () => {
    const cleanup = vi.fn();
    const effect = vi.fn(() => cleanup);

    const { rerender, unmount } = renderHook(
      ({ dep }) => useUpdateEffect(effect, [dep]),
      { initialProps: { dep: 0 } },
    );

    rerender({ dep: 1 });
    expect(cleanup).not.toHaveBeenCalled();

    rerender({ dep: 2 });
    expect(cleanup).toHaveBeenCalledTimes(1);

    unmount();
    expect(cleanup).toHaveBeenCalledTimes(2);
  });
});
