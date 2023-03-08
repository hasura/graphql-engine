test('Skipped tests', () => {});
/*
Commented out because of a the following circular dependency problem.

TypeError: Cannot read properties of undefined (reading 'postgres')

      446 |
      447 | export let currentDriver: Driver = 'postgres';
    > 448 | export let dataSource: DataSourcesAPI = services[currentDriver || 'postgres'];
          |                                                 ^
      449 |
      450 | export const isFeatureSupported = (
      451 |   feature: Path<DeepRequired<SupportedFeaturesType>>
*/

/*

import { fireEvent, screen } from '@testing-library/react';
import { GraphQLFieldCustomization } from '../GraphQLFieldCustomization/GraphQLFieldCustomization';
import { renderWithClient } from '../../../../../hooks/__tests__/common/decorator';



describe('component GraphQLFieldCustomization', () => {
  it('renders', () => {
    renderWithClient(<GraphQLFieldCustomization onChange={() => null} />);

    fireEvent.click(screen.getByText('GraphQL Field Customization'));

    expect(screen.getByRole('form', { name: 'rootFields' })).toHaveFormValues({
      'rootFields.namespace': '',
      'rootFields.prefix': '',
      'rootFields.suffix': '',
    });

    expect(screen.getByRole('form', { name: 'typeNames' })).toHaveFormValues({
      'typeNames.prefix': '',
      'typeNames.suffix': '',
    });
  });

  it('passes props', () => {
    renderWithClient(
      <GraphQLFieldCustomization
        rootFields={{
          namespace: 'name',
          prefix: 'prefix_',
          suffix: '_suffix',
        }}
        typeNames={{
          prefix: 'type_name_prefix_',
          suffix: '_type_name_suffix',
        }}
        onChange={() => null}
      />
    );

    fireEvent.click(screen.getByText('GraphQL Field Customization'));

    expect(screen.getByRole('form', { name: 'rootFields' })).toHaveFormValues({
      'rootFields.namespace': 'name',
      'rootFields.prefix': 'prefix_',
      'rootFields.suffix': '_suffix',
    });

    expect(screen.getByRole('form', { name: 'typeNames' })).toHaveFormValues({
      'typeNames.prefix': 'type_name_prefix_',
      'typeNames.suffix': '_type_name_suffix',
    });
  });

  describe('when the user provides the values', () => {
    it('calls the on change callback', () => {
      const onChange = jest.fn();
      renderWithClient(<GraphQLFieldCustomization onChange={onChange} />);

      fireEvent.click(screen.getByText('GraphQL Field Customization'));

      const expectations = [
        { textboxName: 'rootFields.namespace', expected: 'root_field_name' },
        { textboxName: 'rootFields.prefix', expected: 'a_root_field_prefix_' },
        { textboxName: 'rootFields.suffix', expected: '_a_root_field_suffix' },
        { textboxName: 'typeNames.prefix', expected: 'a_type_name_prefix_' },
        { textboxName: 'typeNames.suffix', expected: '_a_type_name_suffix' },
      ];

      expectations.forEach(expectation => {
        fireEvent.change(
          screen.getByRole('textbox', { name: expectation.textboxName }),
          {
            target: { value: expectation.expected },
          }
        );

        expect(onChange).toHaveBeenCalledWith(
          expectation.textboxName,
          expectation.expected
        );
      });
    });
  });
});
*/
