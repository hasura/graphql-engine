import React from 'react';

type PreviewTableProps = {
  headings: {
    content: string;
    className?: string;
  }[];
};

const PreviewTable: React.FC<PreviewTableProps> = ({ headings, children }) => (
  <table className="w-full border-collapse mb-md">
    <tr className="border-b border-gray-300">
      {headings.map(heading => (
        <th className={heading?.className}>{heading.content}</th>
      ))}
    </tr>
    <tbody>{children}</tbody>
  </table>
);

export default PreviewTable;
