import { Meta, StoryObj } from '@storybook/react-webpack5';
import { Table } from './Table';

export default {
  title: 'components/table/Table',
  parameters: {
    docs: {
      description: {
        component: `A thin re-export of [Radix Themes' Table](https://www.radix-ui.com/themes/docs/components/table) primitives (\`Root\`, \`Header\`, \`Body\`, \`Row\`, \`Cell\`, \`ColumnHeaderCell\`, \`RowHeaderCell\`).`,
      },
      source: { type: 'code' },
    },
  },
  decorators: [(Story) => <div className="p-4">{Story()}</div>],
} as Meta;

export const Basic: StoryObj = {
  render: () => (
    <Table.Root>
      <Table.Header>
        <Table.Row>
          <Table.ColumnHeaderCell>Name</Table.ColumnHeaderCell>
          <Table.ColumnHeaderCell>Type</Table.ColumnHeaderCell>
          <Table.ColumnHeaderCell>Nullable</Table.ColumnHeaderCell>
        </Table.Row>
      </Table.Header>
      <Table.Body>
        <Table.Row>
          <Table.RowHeaderCell>id</Table.RowHeaderCell>
          <Table.Cell>uuid</Table.Cell>
          <Table.Cell>false</Table.Cell>
        </Table.Row>
        <Table.Row>
          <Table.RowHeaderCell>name</Table.RowHeaderCell>
          <Table.Cell>text</Table.Cell>
          <Table.Cell>true</Table.Cell>
        </Table.Row>
      </Table.Body>
    </Table.Root>
  ),

  name: '🧰 Basic',

  parameters: {
    docs: {
      source: { state: 'open' },
    },
  },
};
