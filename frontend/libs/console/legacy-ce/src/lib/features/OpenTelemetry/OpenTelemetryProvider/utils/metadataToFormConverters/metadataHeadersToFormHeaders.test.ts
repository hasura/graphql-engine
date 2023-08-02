import {
  formHeadersToMetadataHeaders,
  metadataHeadersToFormHeaders,
} from './metadataHeadersToFormHeaders';

describe('Metadata <--> Form Values', () => {
  describe.each`
    metadataHeaders                                                                     | formValues
    ${[]}                                                                               | ${[]}
    ${[{ name: 'foo', value: 'bar' }]}                                                  | ${[{ name: 'foo', value: 'bar', type: 'from_value' }]}
    ${[{ name: 'foo', value: 'bar' }, { name: 'baz', value: 'qux' }]}                   | ${[{ name: 'foo', value: 'bar', type: 'from_value' }, { name: 'baz', value: 'qux', type: 'from_value' }]}
    ${[{ name: 'foo', value_from_env: 'bar' }]}                                         | ${[{ name: 'foo', value: 'bar', type: 'from_env' }]}
    ${[{ name: 'foo', value_from_env: 'bar' }, { name: 'baz', value_from_env: 'qux' }]} | ${[{ name: 'foo', value: 'bar', type: 'from_env' }, { name: 'baz', value: 'qux', type: 'from_env' }]}
    ${[{ name: 'foo', value_from_env: 'bar' }, { name: 'baz', value: 'qux' }]}          | ${[{ name: 'foo', value: 'bar', type: 'from_env' }, { name: 'baz', value: 'qux', type: 'from_value' }]}
  `(
    // TODO: fix the test description variables
    'Metadata $metadataHeaders <--> Form Values $formValues conversion',
    ({ metadataHeaders, formValues }) => {
      it('Given the metadata, should return the form values', () => {
        expect(metadataHeadersToFormHeaders(metadataHeaders)).toEqual(
          formValues
        );
      });

      it('Given the form values, should return the metadata', () => {
        expect(formHeadersToMetadataHeaders(formValues)).toEqual(
          metadataHeaders
        );
      });
    }
  );

  describe.each`
    metadataHeaders                                                                     | formValues
    ${[{ name: 'foo', value: 'bar' }, { name: 'foo', value: 'qux' }]}                   | ${[{ name: 'foo', value: 'bar', type: 'from_value' }, { name: 'foo', value: 'qux', type: 'from_value' }]}
    ${[{ name: 'foo', value_from_env: 'bar' }, { name: 'foo', value_from_env: 'qux' }]} | ${[{ name: 'foo', value: 'bar', type: 'from_env' }, { name: 'foo', value: 'qux', type: 'from_env' }]}
    ${[{ name: 'foo', value_from_env: 'bar' }, { name: 'foo', value: 'qux' }]}          | ${[{ name: 'foo', value: 'bar', type: 'from_env' }, { name: 'foo', value: 'qux', type: 'from_value' }]}
  `(
    // TODO: fix the test description variables
    'When there are duplicated headers, should handle them. Given $metadataHeaders and $formValues',
    ({ metadataHeaders, formValues }) => {
      it('Given the metadata, should return the form values', () => {
        expect(metadataHeadersToFormHeaders(metadataHeaders)).toEqual(
          formValues
        );
      });

      it('Given the form values, should return the metadata', () => {
        expect(formHeadersToMetadataHeaders(formValues)).toEqual(
          metadataHeaders
        );
      });
    }
  );

  describe('Nameless headers in form values', () => {
    test.each`
      formValues                                                                                                                                             | metadataHeaders
      ${[{ name: '', value: '', type: 'from_value' }]}                                                                                                       | ${[]}
      ${[{ name: '', value: '', type: 'from_value' }, { name: '', value: '', type: 'from_value' }]}                                                          | ${[]}
      ${[{ name: 'foo', value: 'bar', type: 'from_value' }, { name: '', value: '', type: 'from_value' }]}                                                    | ${[{ name: 'foo', value: 'bar' }]}
      ${[{ name: '', value: '', type: 'from_value' }, { name: 'foo', value: 'bar', type: 'from_value' }]}                                                    | ${[{ name: 'foo', value: 'bar' }]}
      ${[{ name: 'foo', value: 'bar', type: 'from_value' }, { name: '', value: '', type: 'from_value' }, { name: 'baz', value: 'qux', type: 'from_value' }]} | ${[{ name: 'foo', value: 'bar' }, { name: 'baz', value: 'qux' }]}
    `(
      'when invoked with $formValues, should return $metadataHeaders',
      ({ metadataHeaders, formValues }) => {
        expect(formHeadersToMetadataHeaders(formValues)).toEqual(
          metadataHeaders
        );
      }
    );
  });

  describe('Valueless headers in form values', () => {
    test.each`
      formValues                                          | metadataHeaders
      ${[{ name: 'foo', value: '', type: 'from_value' }]} | ${[{ name: 'foo', value: '' }]}
    `(
      'when invoked with $formValues, should return $metadataHeaders',
      ({ metadataHeaders, formValues }) => {
        expect(formHeadersToMetadataHeaders(formValues)).toEqual(
          metadataHeaders
        );
      }
    );
  });
});
