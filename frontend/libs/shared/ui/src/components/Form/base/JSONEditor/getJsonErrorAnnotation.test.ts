import {
  getJsonAnnotations,
  getJsonErrorAnnotation,
} from './getJsonErrorAnnotation';

describe('getJsonErrorAnnotation', () => {
  it('points at the line of the reported position', () => {
    const text = '{\n  "a": 1,\n  "b": }';

    expect(
      getJsonErrorAnnotation(
        text,
        new SyntaxError('Unexpected token } at position 19'),
      ),
    ).toMatchObject({ row: 2, column: 7 });
  });

  it('uses the reported line and column', () => {
    expect(
      getJsonErrorAnnotation(
        '',
        new SyntaxError('Unexpected token (line 3 column 4)'),
      ),
    ).toMatchObject({ row: 2, column: 3 });
  });

  it('marks the end of an incomplete input', () => {
    const text = '{\n  "a": 1';

    expect(
      getJsonErrorAnnotation(
        text,
        new SyntaxError('Unexpected end of JSON input'),
      ),
    ).toMatchObject({ row: 1, column: 8 });
  });

  it('leaves the quoted input out of the message', () => {
    const text = '{"a": }';
    let error: unknown;
    try {
      JSON.parse(text);
    } catch (e) {
      error = e;
    }

    expect(getJsonErrorAnnotation(text, error).text).not.toContain(text);
  });
});

describe('getJsonAnnotations', () => {
  it('has no annotations for valid JSON', () => {
    expect(getJsonAnnotations('{"a": 1}')).toEqual([]);
  });

  it('marks invalid JSON', () => {
    expect(getJsonAnnotations('{\n  "a" 1\n}')).toEqual([
      expect.objectContaining({ row: 1, type: 'error' }),
    ]);
  });

  it('marks a blank text unless it is allowed', () => {
    expect(getJsonAnnotations(' ')).toHaveLength(1);
    expect(getJsonAnnotations(' ', { allowEmpty: true })).toEqual([]);
  });
});
