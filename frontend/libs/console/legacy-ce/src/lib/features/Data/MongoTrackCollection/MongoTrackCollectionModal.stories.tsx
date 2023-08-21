import { StoryObj, Meta } from '@storybook/react';
import {
  MongoTrackCollectionModal,
  MongoTrackCollectionModalProps,
} from './MongoTrackCollectionModal';
import { ReactQueryDecorator } from '../../../storybook/decorators/react-query';

export default {
  component: MongoTrackCollectionModal,
  argTypes: {
    onSubmit: { action: true },
    onClose: { action: true },
  },
  decorators: [ReactQueryDecorator()],
} as Meta<typeof MongoTrackCollectionModal>;

export const Primary: StoryObj<MongoTrackCollectionModalProps> = {
  args: {
    dataSourceName: 'mongodb',
    collectionName: 'products',
    isVisible: true,
    logicalModels: [
      {
        name: 'logical_model_1',
        fields: [
          {
            name: 'id',
            type: { scalar: 'string', nullable: false },
          },
          {
            name: 'name',
            type: { scalar: 'string', nullable: true },
          },
        ],
      },
      {
        name: 'logical_model_2',
        fields: [
          {
            name: 'id',
            type: { scalar: 'string', nullable: false },
          },
        ],
      },
    ],
  },
};
