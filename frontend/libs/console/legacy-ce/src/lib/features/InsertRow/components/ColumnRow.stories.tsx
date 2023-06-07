import { Meta, StoryFn } from '@storybook/react';
import { handlers } from '../../../mocks/metadata.mock';
import { ColumnRow } from './ColumnRow';
import { action } from '@storybook/addon-actions';

export default {
  title: 'Data/Insert Row/components/ColumnRow',
  component: ColumnRow,
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof ColumnRow>;

const Template: StoryFn<typeof ColumnRow> = args => (
  <div className="max-w-screen-md">
    <ColumnRow {...args} />
  </div>
);

export const Base = {
  render: Template,

  args: {
    onChange: () => action('onChange')(),
    label: 'id',
    name: 'id',
    isDisabled: false,
    isNullDisabled: false,
    isDefaultDisabled: false,
    resetToken: '',
    placeholder: 'placeholder...',
    dataType: 'number',
  },
};

export const Disabled = {
  render: Template,

  args: {
    ...Base.args,
    isDisabled: true,
  },
};

export const NotNullable = {
  render: Template,

  args: {
    ...Base.args,
    isNullDisabled: true,
  },
};

export const NoDefaultValue = {
  render: Template,

  args: {
    ...Base.args,
    isDefaultDisabled: true,
  },
};

export const StringColumn = {
  render: Template,

  args: {
    ...Base.args,
    dataType: 'string',
    placeholder: 'string...',
  },
};

export const JsonColumn = {
  render: Template,

  args: {
    ...Base.args,
    dataType: 'json',
    placeholder: 'json...',
  },
};
