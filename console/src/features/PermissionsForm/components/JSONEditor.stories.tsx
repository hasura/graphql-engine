import React from 'react';
import { Story, Meta } from '@storybook/react';

import { JSONEditor, JSONEditorProps } from './JSONEditor';

export default {
  title: 'Permissions Form/Components/JSON Editor',
  component: JSONEditor,
} as Meta;

export const Default: Story<JSONEditorProps> = args => <JSONEditor {...args} />;
Default.args = {
  initData: '{"id":{"_eq":1}}',
};
