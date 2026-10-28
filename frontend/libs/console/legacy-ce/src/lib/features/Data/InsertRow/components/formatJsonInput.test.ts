import { describe, expect, it } from 'vitest';
import { formatJsonInput } from './formatJsonInput';

describe('formatJsonInput', () => {
  it('pretty-prints nested objects and arrays with two-space indentation', () => {
    const value = '{"nested":{"items":[1,true,null,{},[]]},"empty":{}}';
    expect(formatJsonInput(value, true)).toBe(
      JSON.stringify(JSON.parse(value), null, 2),
    );
    expect(formatJsonInput(formatJsonInput(value, true), false)).toBe(value);
  });

  it('preserves large numbers, duplicate keys, escapes and string whitespace', () => {
    const value = String.raw`{ "id": 9007199254740993, "n": 1.2300e+40, "id": -0, "text": "a  b\n\"c\\d", "unicode": "\u0061" }`;
    const compact = String.raw`{"id":9007199254740993,"n":1.2300e+40,"id":-0,"text":"a  b\n\"c\\d","unicode":"\u0061"}`;
    expect(formatJsonInput(value, false)).toBe(compact);
    expect(formatJsonInput(formatJsonInput(value, true), false)).toBe(compact);
  });

  it.each(['', '   ', '{"unfinished":', '{invalid}', 'plain text'])(
    'preserves empty or invalid input %j in either mode',
    (value) => {
      expect(formatJsonInput(value, true)).toBe(value);
      expect(formatJsonInput(value, false)).toBe(value);
    },
  );

  it.each(['null', 'false', '0', '"a  b"', '[]', '{}'])(
    'supports JSON scalar and empty collection %s',
    (value) => {
      expect(formatJsonInput(` ${value} `, true)).toBe(value);
      expect(formatJsonInput(` ${value} `, false)).toBe(value);
    },
  );
});
