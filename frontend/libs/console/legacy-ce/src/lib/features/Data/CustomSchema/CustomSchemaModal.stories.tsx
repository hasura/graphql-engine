import { ComponentMeta, Story } from '@storybook/react';
import { CustomSchemaModal, CustomSchemaModalProps } from './CustomSchemaModal';

export default {
  component: CustomSchemaModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
} as ComponentMeta<typeof CustomSchemaModal>;

export const Primary: Story<CustomSchemaModalProps> = args => (
  <CustomSchemaModal {...args} />
);

Primary.args = {
  tableName: 'Customer',
  callToAction: 'Customize & Track',
  callToDeny: 'Cancel',
  callToActionLoadingText: 'Saving...',
  jsonSchema: JSON.stringify(
    {
      title: 'movie',
      properties: {
        _id: { bsonType: 'objectId' },
        title: { bsonType: 'string' },
        year: { bsonType: 'int' },
        director: { bsonType: 'int' },
      },
    },
    null,
    2
  ),
  graphqlSchema: `
    type Movie {
      _id: ID!
      title: String!
      year: Int!
      director: Int!
    }
  `,
};
