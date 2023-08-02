import {
  encodeFileContent,
  isTypedObject,
  TypedObjectValidator,
} from './jsUtils';

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

type CustomType = {
  value: string;
};

type MyCustomDataset = {
  dataset: string;
};

describe('isTypedObject', () => {
  it('return true if it matches the typed object', () => {
    const myObject: unknown = {
      value: '1',
    };

    const validator: TypedObjectValidator = _object => 'value' in _object;
    const isMyObject = isTypedObject<CustomType>(myObject, validator);

    expect(isMyObject).toBe(true);

    if (isMyObject) {
      // The type of myObject is CustomType
      // TS provides autocompletion on myObject
      expect(myObject.value).toBe('1');
    }
  });

  it("return false if it doesn't match the typed object", () => {
    const myObject: unknown = {
      schema: '1',
    };

    const validator: TypedObjectValidator = _object => 'dataset' in _object;
    const isDataset = isTypedObject<MyCustomDataset>(myObject, validator);

    expect(isDataset).toBe(false);
    // The type of myObject is still unknown
  });
});
