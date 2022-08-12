import { isPostgresFunction } from '../utils';

describe('isPostgresFunction util function should return', () => {
  test('false for string values', () => {
    const res = isPostgresFunction('simplestring');
    expect(res).toBe(false);
  });
  test('false for number values', () => {
    const res = isPostgresFunction(1234);
    expect(res).toBe(false);
  });
  test('true for empty sql functions', () => {
    const res = isPostgresFunction('someSQLFunction()');
    expect(res).toBe(true);
  });
  test('true for sql functions with params', () => {
    const res = isPostgresFunction('someSQLFunction(param1, "param2")');
    expect(res).toBe(true);
  });
});
