import type { HtmlAnalyticsAttributes } from '../types';
import type { AnalyticsOptions } from './getAnalyticsAttributes';
import { getAnalyticsAttributes } from './getAnalyticsAttributes';
import { REDACT_EVERYTHING } from './heap/getRedactAttributes';

describe('getAnalyticsAttributes', () => {
  it('When invoked with a name, then should include the data-analytics-name attribute', () => {
    const expected: HtmlAnalyticsAttributes = {
      'data-analytics-name': 'my-name',
    };

    expect(getAnalyticsAttributes('my-name')).toEqual(expected);
  });

  it('When invoked with a name and legacyTrackIdAttribute, then should include both data-analytics-name and data-trackid attributes', () => {
    const options: AnalyticsOptions = {
      legacyTrackIdAttribute: true,
    };
    const expected: HtmlAnalyticsAttributes = {
      'data-analytics-name': 'my-name',
      'data-trackid': 'my-name',
    };

    expect(getAnalyticsAttributes('my-name', options)).toEqual(expected);
  });

  it('When invoked with a name and some redact options, then should include the data-analytics-name attribute and the data redact attributes', () => {
    // Please note that all the thorough tests for the redact options can be found in the getRedactAttributes' tests
    const options: AnalyticsOptions = {
      redactText: true,
    };
    const expected: HtmlAnalyticsAttributes = {
      'data-analytics-name': 'my-name',
      'data-heap-redact-text': true,
    };

    expect(getAnalyticsAttributes('my-name', options)).toEqual(expected);
  });

  it('When invoked with the special `REDACT_EVERYTHING` object, then should include every know attribute that could contain sensitive data', () => {
    const expected: HtmlAnalyticsAttributes = {
      'data-analytics-name': 'my-name',
      'data-heap-redact-text': true,
      'data-heap-redact-attributes':
        'id,title,data-test,data-element,data-cy,data-testid,data-index-id,data-key,href,value,name,key,placeholder,for',
    };

    expect(getAnalyticsAttributes('my-name', REDACT_EVERYTHING)).toEqual(
      expected
    );
  });
});
