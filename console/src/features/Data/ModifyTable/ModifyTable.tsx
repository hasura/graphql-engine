import React from 'react';
import { ManageTableProps } from '../ManageTable/ManageTable';
import { TableColumns, TableComments } from './components';
import { Section } from './parts';

export const ModifyTable: React.VFC<ManageTableProps> = props => {
  return (
    <div className="w-full 2xl:w-3/4 m-2 bg-white p-4 rounded-sm border ">
      <Section headerText="Table Comments">
        <TableComments {...props} />
      </Section>
      <Section headerText="Table Columns">
        <TableColumns {...props} />
      </Section>
    </div>
  );
};
