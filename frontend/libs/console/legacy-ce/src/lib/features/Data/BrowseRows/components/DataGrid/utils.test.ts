import { formatCellValue, MAX_CELL_TEXT_LENGTH } from './utils';

describe('formatCellValue', () => {
  it('keeps short values as they are', () => {
    expect(formatCellValue('hello')).toBe('hello');
    expect(formatCellValue(42)).toBe('42');
    expect(formatCellValue({ a: 1 })).toBe('{"a":1}');
    expect(formatCellValue(null)).toBe('null');
  });

  it('truncates very long values, e.g. a large bytea', () => {
    const bytea = `\\x${'ab'.repeat(1_000_000)}`;
    const formatted = formatCellValue(bytea);

    expect(formatted?.length).toBeLessThan(MAX_CELL_TEXT_LENGTH + 50);
    expect(formatted?.startsWith(bytea.slice(0, MAX_CELL_TEXT_LENGTH))).toBe(
      true,
    );
    expect(formatted).toContain('characters)');
  });
});
