import { jsonToSchema } from '@walmartlabs/json-to-simple-graphql-schema/lib';
import {
  prepareJsonSample,
  sampleErrorMessage,
  UnsupportedSampleError,
} from './utils';

const generate = (json: string) =>
  jsonToSchema({
    jsonInput: prepareJsonSample(json),
    baseType: 'SampleOutput',
  }).value;

describe('prepareJsonSample', () => {
  it('leaves objects untouched', () => {
    const json = '{"id": 1, "name": "x"}';
    expect(prepareJsonSample(json)).toBe(json);
  });

  it('leaves invalid JSON untouched', () => {
    expect(prepareJsonSample('{"id": ')).toBe('{"id": ');
  });

  it('generates the item type for an array of objects', () => {
    expect(generate('[{"id": 1}, {"name": "x"}]')).toBe(
      'type SampleOutput {\n  id: Int\n  name: String\n}',
    );
  });

  it('keeps nested types of array items', () => {
    expect(generate('[{"id": 1, "tags": [{"label": "a"}]}]')).toBe(
      'type Tags {\n  label: String\n}\n\ntype SampleOutput {\n  id: Int\n  tags: [Tags]\n}',
    );
  });

  it.each([
    ['[]', 'Add at least one item to the array to generate a type'],
    ['[1, 2]', 'Only arrays of objects can be converted to a type'],
    ['[{"id": 1}, null]', 'Only arrays of objects can be converted to a type'],
    ['{}', 'Add at least one field to the object to generate a type'],
    ['[{}]', 'Add at least one field to the object to generate a type'],
  ])('explains why %s has no type', (json, message) => {
    expect(() => prepareJsonSample(json)).toThrow(UnsupportedSampleError);
    expect(() => prepareJsonSample(json)).toThrow(message);
  });
});

describe('sampleErrorMessage', () => {
  it('shows the reason for unsupported samples', () => {
    expect(sampleErrorMessage(new UnsupportedSampleError('reason'))).toBe(
      'reason',
    );
  });

  it('reports other errors as invalid JSON', () => {
    expect(sampleErrorMessage(new SyntaxError('Unexpected end'))).toBe(
      'Invalid JSON',
    );
  });
});
