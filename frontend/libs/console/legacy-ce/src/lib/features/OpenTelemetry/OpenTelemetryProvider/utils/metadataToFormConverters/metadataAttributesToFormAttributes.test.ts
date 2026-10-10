import {
  formAttributesToMetadataAttributes,
  metadataAttributesToFormAttributes,
} from './metadataAttributesToFormAttributes';

describe('Metadata <--> Form Values', () => {
  describe.each`
    metadataAttributes                                                | formValues
    ${[]}                                                             | ${[]}
    ${[{ name: 'foo', value: 'bar' }]}                                | ${[{ name: 'foo', value: 'bar', type: 'value' }]}
    ${[{ name: 'foo', value: 'bar' }, { name: 'baz', value: 'qux' }]} | ${[{ name: 'foo', value: 'bar', type: 'value' }, { name: 'baz', value: 'qux', type: 'value' }]}
  `(
    // TODO: fix the test description variables
    'Metadata $metadataAttributes <--> Form Values $formValues conversion',
    ({ metadataAttributes, formValues }) => {
      it('Given the metadata, should return the form values', () => {
        expect(metadataAttributesToFormAttributes(metadataAttributes)).toEqual(
          formValues,
        );
      });

      it('Given the form values, should return the metadata', () => {
        expect(formAttributesToMetadataAttributes(formValues)).toEqual(
          metadataAttributes,
        );
      });
    },
  );

  describe.each`
    metadataAttributes                                                | formValues
    ${[{ name: 'foo', value: 'bar' }, { name: 'foo', value: 'qux' }]} | ${[{ name: 'foo', value: 'bar', type: 'value' }, { name: 'foo', value: 'qux', type: 'value' }]}
  `(
    // TODO: fix the test description variables
    'When there are duplicated attributes, should handle them. Given $metadataAttributes and $formValues',
    ({ metadataAttributes, formValues }) => {
      it('Given the metadata, should return the form values', () => {
        expect(metadataAttributesToFormAttributes(metadataAttributes)).toEqual(
          formValues,
        );
      });

      it('Given the form values, should return the metadata', () => {
        expect(formAttributesToMetadataAttributes(formValues)).toEqual(
          metadataAttributes,
        );
      });
    },
  );

  describe('Nameless attributes in form values', () => {
    test.each`
      formValues                                                                                                                              | metadataAttributes
      ${[{ name: '', value: '', type: 'value' }]}                                                                                             | ${[]}
      ${[{ name: '', value: '', type: 'value' }, { name: '', value: '', type: 'value' }]}                                                     | ${[]}
      ${[{ name: 'foo', value: 'bar', type: 'value' }, { name: '', value: '', type: 'value' }]}                                               | ${[{ name: 'foo', value: 'bar' }]}
      ${[{ name: '', value: '', type: 'value' }, { name: 'foo', value: 'bar', type: 'value' }]}                                               | ${[{ name: 'foo', value: 'bar' }]}
      ${[{ name: 'foo', value: 'bar', type: 'value' }, { name: '', value: '', type: 'value' }, { name: 'baz', value: 'qux', type: 'value' }]} | ${[{ name: 'foo', value: 'bar' }, { name: 'baz', value: 'qux' }]}
    `(
      'when invoked with $formValues, should return $metadataAttributes',
      ({ metadataAttributes, formValues }) => {
        expect(formAttributesToMetadataAttributes(formValues)).toEqual(
          metadataAttributes,
        );
      },
    );
  });

  describe('Valueless attributes in form values', () => {
    test.each`
      formValues                                     | metadataAttributes
      ${[{ name: 'foo', value: '', type: 'value' }]} | ${[{ name: 'foo', value: '' }]}
    `(
      'when invoked with $formValues, should return $metadataAttributes',
      ({ metadataAttributes, formValues }) => {
        expect(formAttributesToMetadataAttributes(formValues)).toEqual(
          metadataAttributes,
        );
      },
    );
  });
});
