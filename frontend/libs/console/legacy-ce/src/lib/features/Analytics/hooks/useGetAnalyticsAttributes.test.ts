import { renderHook } from '@testing-library/react-hooks';

import type { HtmlAnalyticsAttributes } from '../types';

import { REDACT_EVERYTHING } from '../core/heap/getRedactAttributes';
import { useGetAnalyticsAttributes } from './useGetAnalyticsAttributes';

describe('useGetAnalyticsAttributes', () => {
  it('When invoked with the special `REDACT_EVERYTHING` object, then should include every know attribute that could contain sensitive data', () => {
    const expected: HtmlAnalyticsAttributes = {
      'data-analytics-name': 'my-name',
      'data-heap-redact-text': true,
      'data-heap-redact-attributes':
        'id,title,data-test,data-element,data-cy,data-testid,data-index-id,data-key,href,value,name,key,placeholder,for',
    };

    const { result } = renderHook(() =>
      useGetAnalyticsAttributes('my-name', REDACT_EVERYTHING)
    );

    expect(result.current).toEqual(expected);
  });

  it('When invoked without a name, then should return an empty object', () => {
    const { result } = renderHook(() => useGetAnalyticsAttributes());

    expect(result.current).toEqual({});
  });

  it('When invoked with some options but without a name, then should return an empty object', () => {
    const { result } = renderHook(() =>
      useGetAnalyticsAttributes(undefined, {
        redactText: true,
      })
    );

    expect(result.current).toEqual({});
  });

  // Until `useGetAnalyticsAttributes`is just a wrapper around `getAnalyticsAttributes` (that has its own tests),
  // it does not need to be tested thoroughly.
});
