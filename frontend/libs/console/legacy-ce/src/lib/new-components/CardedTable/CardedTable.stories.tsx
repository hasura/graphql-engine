import React from 'react';
import { Meta } from '@storybook/react';
import { CardedTable } from './CardedTable';

export default {
  title: 'components/CardedTable',
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

const dataWithActions = data.map(row => [...row, action]);

export const withActions = () => (
  <CardedTable
    columns={[...columns, null]}
    data={dataWithActions}
    showActionCell
  />
);

export const withoutActions = () => (
  <CardedTable columns={columns} data={data} />
);

export const withHelperComponents = () => (
  <CardedTable.Table>
    <CardedTable.Header columns={columns} />
    <CardedTable.Body data={data} />
  </CardedTable.Table>
);

export const withIndividualComponents = () => (
  <CardedTable.Table>
    <CardedTable.TableHead>
      <CardedTable.TableHeadRow>
        {columns.map(column => (
          <CardedTable.TableHeadCell>{column}</CardedTable.TableHeadCell>
        ))}
      </CardedTable.TableHeadRow>
    </CardedTable.TableHead>
    <CardedTable.TableBody>
      {data.map(row => (
        <CardedTable.TableBodyRow>
          {row.map(cell => (
            <CardedTable.TableBodyCell>{cell}</CardedTable.TableBodyCell>
          ))}
        </CardedTable.TableBodyRow>
      ))}
    </CardedTable.TableBody>
  </CardedTable.Table>
);
