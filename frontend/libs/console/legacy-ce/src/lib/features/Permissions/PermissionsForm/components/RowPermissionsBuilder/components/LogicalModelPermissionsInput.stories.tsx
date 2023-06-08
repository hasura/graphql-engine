import { Meta, StoryObj } from '@storybook/react';
import { action } from '@storybook/addon-actions';

import { RowPermissionsInput } from './RowPermissionsInput';
import { comparators } from './__tests__/fixtures/comparators';
import { handlers } from './__tests__/fixtures/jsonb/handlers';
import { ReactQueryDecorator } from '../../../../../../storybook/decorators/react-query';

export default {
  title: 'Features/Permissions/Form/Logical Model Permissions Input',
  component: RowPermissionsInput,
  parameters: {
    msw: handlers(),
  },
  decorators: [ReactQueryDecorator()],
} as Meta;

type Story = StoryObj<typeof RowPermissionsInput>;

export const Basic: Story = {
  args: {
    table: undefined,
    tables: [],
    onPermissionsChange: action('onPermissionsChange'),
    logicalModels: [
      {
        fields: [
          { name: 'one', nullable: false, type: 'text' },
          { name: 'two', nullable: false, type: 'text' },
        ],
        name: 'hello_world',
        source: {
          name: 'default',
          kind: 'postgres',
          configuration: {},
          tables: [],
        },
      },
      {
        fields: [
          { name: 'a', nullable: false, type: 'text' },
          { name: 'b', nullable: false, type: 'text' },
        ],
        name: 'logical_model',
        source: {
          name: 'default',
          kind: 'postgres',
          configuration: {},
          tables: [],
        },
      },
    ],
    logicalModel: 'hello_world',
    comparators,
    permissions: { one: { _eq: 'eqone' } },
  },
};
