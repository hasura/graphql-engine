import { parseStatementTimeout } from './utils';

describe('parseStatementTimeout', () => {
  it('reads a positive number of seconds', () => {
    expect(parseStatementTimeout(' 25 ')).toBe(25);
    expect(parseStatementTimeout('1.5')).toBe(1.5);
  });

  it.each(['', '0', '-5', 'abc', 'NaN', 'Infinity'])(
    'treats %j as no timeout',
    (value) => {
      expect(parseStatementTimeout(value)).toBe(0);
    },
  );
});
