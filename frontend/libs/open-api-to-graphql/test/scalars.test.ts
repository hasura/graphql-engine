import { test, expect, describe, vi } from 'vitest';
import { parseValue } from 'graphql';
import { GraphQLBigInt, GraphQLJSON } from '../src/scalars';

describe('GraphQLBigInt', () => {
  test('serializes safe integers as numbers', () => {
    expect(GraphQLBigInt.serialize(42)).toBe(42);
    expect(GraphQLBigInt.serialize('42')).toBe(42);
    expect(GraphQLBigInt.serialize(BigInt(10))).toBe(10);
    expect(GraphQLBigInt.serialize(Object(7))).toBe(7);
  });

  test('keeps unsafe integers exact', () => {
    // Warns once that BigInts are serialized as strings without a JSON patch
    const warn = vi.spyOn(console, 'warn').mockImplementation(() => undefined);
    const serialized = GraphQLBigInt.serialize('9007199254740993');
    expect(String(serialized)).toBe('9007199254740993');
    warn.mockRestore();
  });

  test('rejects non-integer values', () => {
    expect(() => GraphQLBigInt.serialize(1.5)).toThrow(
      'BigInt cannot represent non-integer value: 1.5',
    );
    expect(() => GraphQLBigInt.serialize(null)).toThrow(
      'BigInt cannot represent non-integer value: null',
    );
  });

  test('parses values and literals to bigint', () => {
    expect(GraphQLBigInt.parseValue('9007199254740993')).toBe(
      BigInt('9007199254740993'),
    );
    expect(GraphQLBigInt.parseLiteral(parseValue('"42"'))).toBe(BigInt(42));
    expect(() => GraphQLBigInt.parseLiteral(parseValue('[1]'))).toThrow(
      'BigInt cannot represent non-integer value: [1]',
    );
  });
});

describe('GraphQLJSON', () => {
  test('serializes and parses values as-is', () => {
    const value = { a: [1, { b: 'c' }], d: null };
    expect(GraphQLJSON.serialize(value)).toBe(value);
    expect(GraphQLJSON.parseValue(value)).toBe(value);
  });

  test('parses nested literals and variables', () => {
    expect(
      GraphQLJSON.parseLiteral(
        parseValue('{a: [1, 1.5, "s", true, null], b: $v}'),
        {
          v: { x: 1 },
        },
      ),
    ).toEqual({ a: [1, 1.5, 's', true, null], b: { x: 1 } });
  });
});
