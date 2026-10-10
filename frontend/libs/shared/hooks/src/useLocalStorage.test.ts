// @vitest-environment jsdom
import { act, renderHook } from '@testing-library/react';
import { useLocalStorage } from './useLocalStorage';

describe('useLocalStorage', () => {
  beforeEach(() => {
    window.localStorage.clear();
  });

  it('returns the initial value when localStorage is empty', () => {
    const { result } = renderHook(() => useLocalStorage('my-key', 'default'));

    expect(result.current[0]).toBe('default');
  });

  it('returns the parsed value already in localStorage instead of the initial value', () => {
    window.localStorage.setItem('my-key', JSON.stringify('stored value'));

    const { result } = renderHook(() => useLocalStorage('my-key', 'default'));

    expect(result.current[0]).toBe('stored value');
  });

  it('updates state and persists the new value to localStorage', () => {
    const { result } = renderHook(() => useLocalStorage('my-key', 'default'));

    act(() => {
      result.current[1]('updated value');
    });

    expect(result.current[0]).toBe('updated value');
    expect(window.localStorage.getItem('my-key')).toBe(
      JSON.stringify('updated value'),
    );
  });

  it('falls back to the initial value when the stored value is not valid JSON', () => {
    window.localStorage.setItem('my-key', '{not valid json');

    const { result } = renderHook(() => useLocalStorage('my-key', 'default'));

    expect(result.current[0]).toBe('default');
  });
});
