import { getActionType } from '../GraphQLFieldCustomization/GraphQLFieldCustomizationContainer';
import { CustomizationFieldName } from '../GraphQLFieldCustomization/types';

describe('getActionType', () => {
  const tests = [
    {
      fieldName: 'rootFields.namespace',
      expected: 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_NAMESPACE',
    },
    {
      fieldName: 'rootFields.prefix',
      expected: 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_PREFIX',
    },
    {
      fieldName: 'rootFields.suffix',
      expected: 'UPDATE_CUSTOMIZATION_ROOT_FIELDS_SUFFIX',
    },
    {
      fieldName: 'typeNames.prefix',
      expected: 'UPDATE_CUSTOMIZATION_TYPE_NAMES_PREFIX',
    },
    {
      fieldName: 'typeNames.suffix',
      expected: 'UPDATE_CUSTOMIZATION_TYPE_NAMES_SUFFIX',
    },
  ];

  tests.forEach(test => {
    describe(`when fieldName is ${test.fieldName}`, () => {
      it(`returns ${test.expected}`, () => {
        expect(getActionType(test.fieldName as CustomizationFieldName)).toBe(
          test.expected
        );
      });
    });
  });
});
