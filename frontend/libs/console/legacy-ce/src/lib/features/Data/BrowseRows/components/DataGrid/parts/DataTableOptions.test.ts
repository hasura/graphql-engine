import { formatFilterValue } from './DataTableOptions';

describe('formatFilterValue', () => {
  it.each`
    value                 | expected
    ${[1, 2, 3]}          | ${'("1", "2", "3")'}
    ${['a b', 'c']}       | ${'("a b", "c")'}
    ${'["123","456"]'}    | ${'("123", "456")'}
    ${[]}                 | ${'()'}
    ${'[]'}               | ${'()'}
    ${true}               | ${'true'}
    ${false}              | ${'false'}
    ${'123'}              | ${'"123"'}
    ${42}                 | ${'"42"'}
    ${{ featured: true }} | ${'"{"featured":true}"'}
    ${undefined}          | ${'""'}
  `('formats $value as $expected', ({ value, expected }) => {
    expect(formatFilterValue(value)).toBe(expected);
  });
});
