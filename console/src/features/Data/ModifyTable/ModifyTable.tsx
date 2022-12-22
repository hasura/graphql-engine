import { Table } from '@/features/hasura-metadata-types';
import React from 'react';
import {
  TableColumns,
  TableComments,
  TableRootFields,
  ForeignKeys,
} from './components';
import { Section } from './parts';

export type ModifyTableProps = {
  dataSourceName: string;
  table: Table;
  tableName: string;
};

export const ModifyTable: React.VFC<ModifyTableProps> = props => {
  return (
    <div className="w-full bg-white p-4 rounded-sm border my-2">
      <Section headerText="Table Comments">
        <TableComments {...props} />
      </Section>
      <Section headerText="Table Columns">
        <TableColumns {...props} />
      </Section>
      <Section
        headerText="Foreign Keys"
        tooltipMessage={`
        Foreign keys are one or more columns that point to another table's primary key. They link both tables.
        `}
      >
        <ForeignKeys {...props} />
      </Section>
      <Section
        headerText="Custom Field Names"
        tooltipMessage="Customize table and column root names for GraphQL operations."
      >
        <TableRootFields {...props} />
      </Section>
    </div>
  );
};
