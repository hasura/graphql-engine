// @vitest-environment jsdom
import { renderHook } from '@testing-library/react';
import { vi } from 'vitest';
import { useScrollIntoView, ScrollIntoViewOptions } from './useScrollIntoView';

describe('useScrollIntoView', () => {
  it('scrolls on mount with the provided options', () => {
    const el = document.createElement('div');
    const scrollIntoView = vi.fn();
    el.scrollIntoView = scrollIntoView;
    const ref = { current: el } as React.RefObject<HTMLDivElement>;

    renderHook(() => useScrollIntoView(ref, ['a'], { block: 'start' }));

    expect(scrollIntoView).toHaveBeenCalledTimes(1);
    expect(scrollIntoView).toHaveBeenLastCalledWith({ block: 'start' });
  });

  it('does not re-scroll when deps are unchanged even if a new options object is passed', () => {
    const el = document.createElement('div');
    const scrollIntoView = vi.fn();
    el.scrollIntoView = scrollIntoView;
    const ref = { current: el } as React.RefObject<HTMLDivElement>;

    const { rerender } = renderHook(
      ({ deps, options }) => useScrollIntoView(ref, deps, options),
      {
        initialProps: {
          deps: ['a'] as string[],
          options: { block: 'start' } as ScrollIntoViewOptions,
        },
      },
    );

    expect(scrollIntoView).toHaveBeenCalledTimes(1);

    // same deps, brand-new options object: must NOT trigger another scroll
    rerender({ deps: ['a'], options: { block: 'center' } });
    rerender({ deps: ['a'], options: { block: 'end' } });

    expect(scrollIntoView).toHaveBeenCalledTimes(1);
  });

  it('re-scrolls when deps change, using the latest options', () => {
    const el = document.createElement('div');
    const scrollIntoView = vi.fn();
    el.scrollIntoView = scrollIntoView;
    const ref = { current: el } as React.RefObject<HTMLDivElement>;

    const { rerender } = renderHook(
      ({ deps, options }) => useScrollIntoView(ref, deps, options),
      {
        initialProps: {
          deps: ['a'] as string[],
          options: { block: 'start' } as ScrollIntoViewOptions,
        },
      },
    );

    expect(scrollIntoView).toHaveBeenCalledTimes(1);

    rerender({ deps: ['b'], options: { block: 'end' } });

    expect(scrollIntoView).toHaveBeenCalledTimes(2);
    expect(scrollIntoView).toHaveBeenLastCalledWith({ block: 'end' });
  });
});
