import { ComponentStory, ComponentMeta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { Widget } from './Widget';
import { MODE, RemoteDatabaseRelationship } from '../../types';

export default {
  component: Widget,
  decorators: [ReactQueryDecorator()],
} as ComponentMeta<typeof Widget>;

export const CreateMode: ComponentStory<typeof Widget> = () => (
  <Widget
    dataSourceName="chinook"
    table={{ name: 'Album', schema: 'public' }}
    onSuccess={() => {}}
    onError={() => {}}
    onCancel={() => {}}
  />
);
