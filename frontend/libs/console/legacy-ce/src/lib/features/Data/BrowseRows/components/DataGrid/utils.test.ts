import {
  decodeBinaryValue,
  formatByteSize,
  formatCellValue,
  getBinaryValueSize,
  MAX_CELL_TEXT_LENGTH,
} from './utils';

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

  it('truncates large objects, e.g. a jsonb document', () => {
    const doc = { items: Array.from({ length: 1000 }, (_, i) => ({ i })) };
    const formatted = formatCellValue(doc);

    expect(formatted?.length).toBeLessThan(MAX_CELL_TEXT_LENGTH + 50);
    expect(formatted?.startsWith('{"items":[{"i":0}')).toBe(true);
  });

  it('formats the length with a fixed locale', () => {
    const formatted = formatCellValue('a'.repeat(1_234_567));

    expect(formatted?.endsWith('… (1,234,567 characters)')).toBe(true);
  });

  it('keeps a value exactly at the limit, truncates one past it', () => {
    const atLimit = 'a'.repeat(MAX_CELL_TEXT_LENGTH);

    expect(formatCellValue(atLimit)).toBe(atLimit);
    expect(formatCellValue(`${atLimit}a`)).toBe(
      `${atLimit}… (${(MAX_CELL_TEXT_LENGTH + 1).toLocaleString('en-US')} characters)`,
    );
  });

  it('does not truncate when maxLength is Infinity', () => {
    const long = 'a'.repeat(MAX_CELL_TEXT_LENGTH * 2);

    expect(formatCellValue(long, Infinity)).toBe(long);
  });

  it('handles bigint values, including nested ones', () => {
    expect(formatCellValue(BigInt(10))).toBe('10');
    expect(formatCellValue([BigInt(1), { a: BigInt(2) }])).toBe(
      '["1",{"a":"2"}]',
    );
  });

  it('does not throw on cyclic values', () => {
    const cyclic: Record<string, unknown> = {};
    cyclic['self'] = cyclic;

    expect(formatCellValue(cyclic)).toBe('[object Object]');
  });

  it('returns undefined for undefined', () => {
    expect(formatCellValue(undefined)).toBeUndefined();
  });
});

describe('decodeBinaryValue', () => {
  it.each`
    encoding             | value
    ${'Postgres hex'}    | ${'\\x48656c6c6f'}
    ${'0x prefixed hex'} | ${'0x48656C6C6F'}
    ${'base64'}          | ${'SGVsbG8='}
    ${'plain text'}      | ${'Hello'}
  `('decodes $encoding', ({ value }) => {
    expect(new TextDecoder().decode(decodeBinaryValue(value))).toBe('Hello');
  });
});

describe('getBinaryValueSize', () => {
  it.each`
    value              | size
    ${'\\x48656c6c6f'} | ${5}
    ${'0x48656C6C6F'}  | ${5}
    ${'\\x'}           | ${0}
    ${'SGVsbG8='}      | ${5}
    ${'SGVsbG8h'}      | ${6}
    ${'Hello'}         | ${5}
  `('is $size bytes for $value', ({ value, size }) => {
    expect(getBinaryValueSize(value)).toBe(size);
  });

  it('matches the decoded length', () => {
    const value = `\\x${'ab'.repeat(1000)}`;
    expect(getBinaryValueSize(value)).toBe(decodeBinaryValue(value).length);
  });
});

describe('formatByteSize', () => {
  it.each`
    bytes              | formatted
    ${0}               | ${'0 B'}
    ${1023}            | ${'1023 B'}
    ${1024}            | ${'1.0 KB'}
    ${1536}            | ${'1.5 KB'}
    ${5 * 1024 * 1024} | ${'5.0 MB'}
  `('formats $bytes as $formatted', ({ bytes, formatted }) => {
    expect(formatByteSize(bytes)).toBe(formatted);
  });
});
