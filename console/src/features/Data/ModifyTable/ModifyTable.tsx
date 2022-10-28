import React from 'react';
import { ManageTableProps } from '../ManageTable/ManageTable';
import { TableColumns, TableComments, TableRootFields } from './components';
import { Section } from './parts';

export type ModifyTableProps = ManageTableProps & { tableName: string };

export const ModifyTable: React.VFC<ModifyTableProps> = props => {
  return (
    <div className="w-full bg-white p-4 rounded-sm border ">
      <Section headerText="Table Comments">
        <TableComments {...props} />
      </Section>
      <Section headerText="Table Columns">
        <TableColumns {...props} />
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
