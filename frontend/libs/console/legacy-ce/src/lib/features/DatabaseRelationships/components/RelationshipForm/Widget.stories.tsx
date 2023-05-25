import { StoryFn, Meta } from '@storybook/react';
import { ReactQueryDecorator } from '../../../../storybook/decorators/react-query';
import { Widget } from './Widget';

export default {
  component: Widget,
  decorators: [ReactQueryDecorator()],
} as Meta<typeof Widget>;

export const CreateMode: StoryFn<typeof Widget> = () => (
  <Widget
    dataSourceName="chinook"
    table={{ name: 'Album', schema: 'public' }}
    onSuccess={() => {}}
    onError={() => {}}
    onCancel={() => {}}
  />
);
