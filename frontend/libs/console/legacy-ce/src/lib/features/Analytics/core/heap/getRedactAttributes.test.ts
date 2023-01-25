import type { HtmlRedactAttributes } from './types';
import { getRedactAttributes, REDACT_EVERYTHING } from './getRedactAttributes';

describe('getRedactAttributes', () => {
  it.each`
    value                                                           | expected
    ${{}}                                                           | ${{}}
    ${{ redactText: false }}                                        | ${{}}
    ${{ redactText: true }}                                         | ${{ 'data-heap-redact-text': true }}
    ${{ htmlAttributesToRedact: 'my-attribute' }}                   | ${{ 'data-heap-redact-attributes': 'my-attribute' }}
    ${{ redactText: true, htmlAttributesToRedact: 'my-attribute' }} | ${{ 'data-heap-redact-text': true, 'data-heap-redact-attributes': 'my-attribute' }}
  `(
    `When invoked with '$value', then should return '$expected'`,
    ({ value, expected }) => {
      expect(getRedactAttributes(value)).toEqual(expected);
    }
  );

  it('When invoked with the special `REDACT_EVERYTHING` object, then should include every know attribute that could contain sensitive data', () => {
    const expected: HtmlRedactAttributes = {
      'data-heap-redact-text': true,
      'data-heap-redact-attributes':
        'id,title,data-test,data-element,data-cy,data-testid,data-index-id,data-key,href,value,name,key,placeholder,for',
    };

    expect(getRedactAttributes(REDACT_EVERYTHING)).toEqual(expected);
  });
});
