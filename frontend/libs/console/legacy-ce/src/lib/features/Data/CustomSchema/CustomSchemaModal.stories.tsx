import { StoryObj, Meta } from '@storybook/react';
import { CustomSchemaModal, CustomSchemaModalProps } from './CustomSchemaModal';

export default {
  component: CustomSchemaModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
} as Meta<typeof CustomSchemaModal>;

export const Primary: StoryObj<CustomSchemaModalProps> = {
  args: {
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
  },
};
