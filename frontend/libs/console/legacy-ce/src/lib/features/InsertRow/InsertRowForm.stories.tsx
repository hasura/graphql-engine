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
    dataType: 'string',
    consoleDataType: 'string',
    config: {
      comment: '',
    },
    isPrimaryKey: true,
  },
  {
    name: 'name',
    dataType: 'string',
    consoleDataType: 'string',
    config: {
      comment: '',
    },
  },
  {
    name: 'surname',
    dataType: 'string',
    consoleDataType: 'string',
    config: {
      comment: '',
    },
    nullable: true,
  },
];

Base.args = {
  columns,
  isLoading: false,
  isInserting: false,
  onInsertRow: () => action('onInsertRow')(),
};
