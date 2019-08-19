import React from 'react';

const TableHeader = ({ headings }) => {
  return (
    <thead>
      <tr>
        {headings.map((heading, index) => (
          <td key={index}>{heading}</td>
        ))}
      </tr>
    </thead>
  );
};

export default TableHeader;
