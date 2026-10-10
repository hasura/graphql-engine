// @vitest-environment jsdom
import { renderHook } from '@testing-library/react';
import { useDocumentTitle } from './useDocumentTitle';

describe('useDocumentTitle', () => {
  it('sets document.title on mount', () => {
    renderHook(() => useDocumentTitle('My Page'));

    expect(document.title).toBe('My Page');
  });

  it('updates document.title when the title prop changes', () => {
    const { rerender } = renderHook(({ title }) => useDocumentTitle(title), {
      initialProps: { title: 'First' },
    });

    expect(document.title).toBe('First');

    rerender({ title: 'Second' });

    expect(document.title).toBe('Second');
  });

  it('restores the previous title on unmount', () => {
    document.title = 'Original Title';

    const { unmount } = renderHook(() => useDocumentTitle('My Page'));

    expect(document.title).toBe('My Page');

    unmount();

    expect(document.title).toBe('Original Title');
  });
});
