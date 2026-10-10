import { Meta } from '@storybook/react-webpack5';
import { CardedTable } from './CardedTable';
import { Table } from '../Table';

export default {
  title: 'components/table/CardedTable',
  component: CardedTable,
} as Meta<typeof CardedTable>;

const columns = ['name', 'title', 'email', 'role'];

const action = (
  <a href="#!" className="text-secondary">
    Edit
  </a>
);

const data = [
  [
    <a href="!#" className="text-secondary">
      Jane Cooper
    </a>,
    'Regional Paradigm Technician',
    'jane.cooper@example.com',
    'Admin',
  ],
  [
    <a href="!#" className="text-secondary">
      Jane Cooper
    </a>,
    'Regional Paradigm Technician',
    'jane.cooper@example.com',
    'Admin',
  ],
  [
    <a href="!#" className="text-secondary">
      Jane Cooper
    </a>,
    'Regional Paradigm Technician',
    'jane.cooper@example.com',
    'Admin',
  ],
];

const dataWithActions = data.map((row) => [...row, action]);

export const withActions = () => (
  <CardedTable columns={[...columns, null]} data={dataWithActions} />
);

export const withoutActions = () => (
  <CardedTable columns={columns} data={data} />
);

export const horizontalTable = () => (
  <CardedTable orientation="horizontal" columns={columns} data={data} />
);

export const withHelperComponents = () => (
  <Table.Root>
    <CardedTable.Header columns={columns} />
    <CardedTable.Body data={data} />
  </Table.Root>
);
