import { encodeFileContent } from './jsUtils';

describe('encodeFileContent', () => {
  it('encodes #', () => {
    expect(encodeFileContent('a#1')).toBe('a%231');
  });

  it('encodes reserved characters', () => {
    expect(encodeFileContent(';,/?:@&=+$')).toBe(
      '%3B%2C%2F%3F%3A%40%26%3D%2B%24'
    );
  });

  it('preserves Unescaped characters', () => {
    expect(encodeFileContent("-_.!~*'()")).toBe("-_.!~*'()");
  });

  it('encodes alphanumeric strings', () => {
    expect(encodeFileContent('example è 123')).toBe('example%20%C3%A8%20123');
  });

  it('encodes newline characters', () => {
    expect(encodeFileContent('sentence\nnewline')).toBe('sentence%0Anewline');
  });

  it('encodes carriage return', () => {
    expect(encodeFileContent('sentence\rnewline')).toBe('sentence%0Dnewline');
  });
});
