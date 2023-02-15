import { ComponentMeta, ComponentStory } from '@storybook/react';
import { handlers } from '@/mocks/metadata.mock';
import { ColumnRow } from './ColumnRow';
import { action } from '@storybook/addon-actions';

export default {
  title: 'Data/Insert Row/components/ColumnRow',
  component: ColumnRow,
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof ColumnRow>;

const Template: ComponentStory<typeof ColumnRow> = args => (
  <div className="max-w-screen-md">
    <ColumnRow {...args} />
  </div>
);

export const Base = Template.bind({});
Base.args = {
  onChange: () => action('onChange')(),
  label: 'id',
  name: 'id',
  isDisabled: false,
  isNullDisabled: false,
  isDefaultDisabled: false,
  resetToken: '',
};

export const Disabled = Template.bind({});
Disabled.args = {
  ...Base.args,
  isDisabled: true,
};

export const NotNullable = Template.bind({});
NotNullable.args = {
  ...Base.args,
  isNullDisabled: true,
};

export const NoDefaultValue = Template.bind({});
NoDefaultValue.args = {
  ...Base.args,
  isDefaultDisabled: true,
};
