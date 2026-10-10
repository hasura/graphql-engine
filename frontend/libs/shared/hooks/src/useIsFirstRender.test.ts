// @vitest-environment jsdom
import { renderHook } from '@testing-library/react';
import { useIsFirstRender } from './useIsFirstRender';

describe('useIsFirstRender', () => {
  it('returns true the first time the returned callback is invoked', () => {
    const { result } = renderHook(() => useIsFirstRender());

    expect(result.current()).toBe(true);
  });

  it('returns false on every subsequent invocation, even across re-renders', () => {
    const { result, rerender } = renderHook(() => useIsFirstRender());

    expect(result.current()).toBe(true);
    expect(result.current()).toBe(false);

    rerender();

    expect(result.current()).toBe(false);
  });
});
