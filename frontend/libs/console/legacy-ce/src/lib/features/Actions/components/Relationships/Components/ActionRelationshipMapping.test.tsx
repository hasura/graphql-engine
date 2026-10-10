import { render } from '@testing-library/react';
import { Theme } from '@radix-ui/themes';
import type { CustomTypeObjectRelationship } from '@hasura/shared/types';
import ActionRelationshipMapping from './ActionRelationshipMapping';

const baseRelationship: CustomTypeObjectRelationship = {
  name: 'user_rel',
  type: 'object',
  source: 'default',
  remote_table: { schema: 'public', name: 'users' },
  field_mapping: { first_field: 'first_column', second_field: 'second_column' },
};

const renderMapping = (relationship: CustomTypeObjectRelationship) =>
  render(
    <Theme>
      <ActionRelationshipMapping
        typeName="SampleType"
        relationship={relationship}
      />
    </Theme>,
  );

describe('ActionRelationshipMapping', () => {
  it('renders the source type name and the "from" field keys', () => {
    const { container } = renderMapping(baseRelationship);

    expect(container).toHaveTextContent(/SampleType/);
    // "from" columns are the keys of the field mapping, comma-joined
    expect(container).toHaveTextContent(/first_field,second_field/);
  });

  it('renders the remote table display name and the "to" field values', () => {
    const { container } = renderMapping(baseRelationship);

    expect(container).toHaveTextContent(/public\.users/);
    // "to" columns are the values of the field mapping, comma-joined
    expect(container).toHaveTextContent(/first_column,second_column/);
  });

  it('handles an empty field mapping without rendering column names', () => {
    const { container } = renderMapping({
      ...baseRelationship,
      field_mapping: {},
    });

    expect(container).toHaveTextContent(/SampleType/);
    expect(container).toHaveTextContent(/public\.users/);
    expect(container).not.toHaveTextContent(/first_field/);
    expect(container).not.toHaveTextContent(/first_column/);
  });
});
