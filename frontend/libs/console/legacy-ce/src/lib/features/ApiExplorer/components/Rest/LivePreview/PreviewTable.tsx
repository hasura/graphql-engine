import { Table } from '@hasura/shared/ui';
import React from 'react';

type PreviewTableProps = {
  headings: {
    content: string;
    className?: string;
  }[];
  children?: React.ReactNode;
};

const PreviewTable: React.FC<PreviewTableProps> = ({ headings, children }) => (
  <Table.Root className="w-full mb-4">
    <Table.Header>
      {headings.map((heading) => (
        <Table.RowHeaderCell
          key={heading.content}
          className={heading?.className}
        >
          {heading.content}
        </Table.RowHeaderCell>
      ))}
    </Table.Header>
    <Table.Body>{children}</Table.Body>
  </Table.Root>
);

export default PreviewTable;
