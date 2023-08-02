import { jsonToString, stringToJson } from './jsonString';

test('Stringifies valid JSON', () => {
  expect(jsonToString({ a: 'b' })).toBe('{"a":"b"}');
});
test('Uses invalid JSON as is', () => {
  expect(jsonToString('{"a":"b"')).toBe('{"a":"b"');
});
test('Parses valid JSON into objects', () => {
  expect(stringToJson('{"a":"b"}')).toEqual({ a: 'b' });
});
test('Leaves invalid JSON as string', () => {
  expect(stringToJson('{"a":"b"')).toBe('{"a":"b"');
});
