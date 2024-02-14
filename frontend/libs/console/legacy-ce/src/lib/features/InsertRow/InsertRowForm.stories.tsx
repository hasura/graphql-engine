import { Meta, StoryObj } from '@storybook/react';
import { handlers } from '../../mocks/metadata.mock';
import { InsertRowForm, InsertRowFormProps } from './InsertRowForm';
import { action } from '@storybook/addon-actions';

export default {
  title: 'Data/Insert Row/components/InsertRowForm',
  component: InsertRowForm,
  parameters: {
    msw: handlers(),
  },
} as Meta<typeof InsertRowForm>;

const columns: InsertRowFormProps['columns'] = [
  {
    name: 'id',
    dataType: 'bigint',
    consoleDataType: 'number',
    config: {
      comment: '',
    },
    isPrimaryKey: true,
    placeholder: 'bigint',
    insertable: true,
    description: '',
    nullable: true,
  },
  {
    name: 'name',
    dataType: 'text',
    consoleDataType: 'string',
    config: {
      comment: '',
    },
    placeholder: 'text',
    insertable: true,
    description: '',
    nullable: true,
  },
  {
    name: 'json',
    dataType: 'jsonb',
    consoleDataType: 'json',
    config: {
      comment: '',
    },
    nullable: true,
    placeholder: '{"name":"john"}',
    insertable: true,
    description: '',
  },
  {
    name: 'date',
    dataType: 'date',
    consoleDataType: 'text',
    config: {
      comment: '',
    },
    nullable: true,
    insertable: true,
    description: '',
    placeholder: '2023-01-01',
  },
  {
    name: 'datetime',
    dataType: 'datetime',
    consoleDataType: 'text',
    config: {
      comment: '',
    },
    nullable: true,
    insertable: true,
    description: '',
    placeholder: '2020-01-14T16:30:00+01:00',
  },
  {
    name: 'time',
    dataType: 'time with time zone',
    consoleDataType: 'text',
    config: {
      comment: '',
    },
    nullable: true,
    insertable: true,
    description: '',
    placeholder: '11:00:00+01:00',
  },
];

export const Base: StoryObj<typeof InsertRowForm> = {
  render: args => <InsertRowForm {...args} />,
  args: {
    columns,
    isLoading: false,
    isInserting: false,
    onInsertRow: () => action('onInsertRow')(),
  },
};
