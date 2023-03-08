import { ComponentMeta, ComponentStory } from '@storybook/react';
import { handlers } from '../../mocks/metadata.mock';
import { InsertRowForm, InsertRowFormProps } from './InsertRowForm';
import { action } from '@storybook/addon-actions';

export default {
  title: 'Data/Insert Row/components/InsertRowForm',
  component: InsertRowForm,
  parameters: {
    msw: handlers(),
  },
} as ComponentMeta<typeof InsertRowForm>;

const Template: ComponentStory<typeof InsertRowForm> = args => (
  <InsertRowForm {...args} />
);

export const Base = Template.bind({});

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
  },
  {
    name: 'name',
    dataType: 'text',
    consoleDataType: 'string',
    config: {
      comment: '',
    },
    placeholder: 'text',
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
  },
  {
    name: 'date',
    dataType: 'date',
    consoleDataType: 'text',
    config: {
      comment: '',
    },
    nullable: true,
    placeholder: '2023-02-01',
  },
];

Base.args = {
  columns,
  isLoading: false,
  isInserting: false,
  onInsertRow: () => action('onInsertRow')(),
};
