import { getOverrideHtmlAttributes } from './getOverrideHtmlAttributes';

describe('getOverrideHtmlAttributes', () => {
  it.each`
    currentHtmlAttributes                                               | htmlAttributes                                                         | expected
    ${{}}                                                               | ${{}}                                                                  | ${[]}
    ${{}}                                                               | ${{ 'data-analytics-name': 'my-name' }}                                | ${[]}
    ${{ 'data-foo': 'bar' }}                                            | ${{ 'data-analytics-name': 'my-name' }}                                | ${[]}
    ${{ 'data-foo': 'my-name' }}                                        | ${{ 'data-analytics-name': 'my-name' }}                                | ${[]}
    ${{ 'data-analytics-name': 'my-name' }}                             | ${{ 'data-analytics-name': 'my-name' }}                                | ${[]}
    ${{ 'data-analytics-name': 'foo' }}                                 | ${{ 'data-analytics-name': 'my-name' }}                                | ${['data-analytics-name']}
    ${{ 'data-analytics-name': 'foo', 'data-heap-redact-text': false }} | ${{ 'data-analytics-name': 'my-name', 'data-heap-redact-text': true }} | ${['data-analytics-name']}
  `(
    'should, given a $currentHtmlAttributes:$currentHtmlAttributes and $htmlAttributes:$htmlAttributes, return $expected as the override attributes',
    ({ currentHtmlAttributes, htmlAttributes, expected }) => {
      expect(
        getOverrideHtmlAttributes(currentHtmlAttributes, htmlAttributes)
      ).toEqual(expected);
    }
  );
});
