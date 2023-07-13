import { StoryObj, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../../storybook/decorators/react-query';
import { ListNativeQueryRelationships } from './ListNativeQueryRelationships';
import { ReduxDecorator } from '../../../../../storybook/decorators/redux-decorator';
import { nativeQueryHandlers } from '../../AddNativeQuery/mocks';

export default {
  component: ListNativeQueryRelationships,
  decorators: [
    ReactQueryDecorator(),
    ReduxDecorator({
      tables: {},
    }),
  ],
  parameters: {
    msw: nativeQueryHandlers({
      metadataOptions: { postgres: { models: true, queries: true } },
      trackNativeQueryResult: 'success',
    }),
    layout: 'fullscreen',
  },
  argTypes: {
    onDeleteRow: { action: 'clicked delete' },
    onEditRow: { action: 'clicked edit' },
  },
} as Meta<typeof ListNativeQueryRelationships>;

export const DefaultView: StoryObj<typeof ListNativeQueryRelationships> = {
  args: {
    dataSourceName: 'postgres',
    nativeQueryName: 'customer_native_query',
  },
  render: args => <ListNativeQueryRelationships {...args} />,
};
