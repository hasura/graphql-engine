import { useEffect } from 'react';

/**
 * Sets `document.title` while the component is mounted, restoring the
 * previous title on unmount. Native replacement for `react-helmet`.
 */
export function useDocumentTitle(title?: string) {
  useEffect(() => {
    if (!title) {
      return;
    }

    const previousTitle = document.title;
    document.title = title;

    return () => {
      document.title = previousTitle;
    };
  }, [title]);
}
