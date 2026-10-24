import {
  formatPostgresArrayLiteral,
  parsePostgresArrayLiteral,
} from './postgresArray';

describe('formatPostgresArrayLiteral', () => {
  it.each`
    values                      | expected
    ${[]}                       | ${'{}'}
    ${[1, 2.5, true]}           | ${'{1,2.5,true}'}
    ${['a', 'b']}               | ${'{a,b}'}
    ${['a b', 'c,d', '']}       | ${'{"a b","c,d",""}'}
    ${['say "hi"', 'back\\sl']} | ${'{"say \\"hi\\"","back\\\\sl"}'}
    ${['null', null]}           | ${'{"null",NULL}'}
    ${[[1, 2], [3, 4]]}         | ${'{{1,2},{3,4}}'}
  `('formats $values as $expected', ({ values, expected }) => {
    expect(formatPostgresArrayLiteral(values)).toBe(expected);
  });
});

describe('parsePostgresArrayLiteral', () => {
  it.each`
    literal                            | expected
    ${'{}'}                            | ${[]}
    ${'{a,b}'}                         | ${['a', 'b']}
    ${'{ 1 , 2 }'}                     | ${['1', '2']}
    ${'{"a b","c,d",""}'}              | ${['a b', 'c,d', '']}
    ${'{"say \\"hi\\"","back\\\\sl"}'} | ${['say "hi"', 'back\\sl']}
    ${'{NULL,"NULL"}'}                 | ${[null, 'NULL']}
  `('parses $literal', ({ literal, expected }) => {
    expect(parsePostgresArrayLiteral(literal)).toEqual(expected);
  });

  it.each(['a,b', '["a"]', '{{1,2}}', '{"unterminated}'])(
    'rejects %s',
    (text) => {
      expect(parsePostgresArrayLiteral(text)).toBeUndefined();
    },
  );

  it('round-trips formatted values', () => {
    const values = ['a b', 'c,d', 'say "hi"', 'back\\sl', '', 'x'];
    expect(
      parsePostgresArrayLiteral(formatPostgresArrayLiteral(values)),
    ).toEqual(values);
  });
});
