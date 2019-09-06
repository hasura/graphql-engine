import React from 'react';

const TableHeader = ({ headings }) => {
  return (
    <thead>
      <tr>
        {headings.map((heading, index) => (
          <th key={index}>{heading}</th>
        ))}
      </tr>
    </thead>
  );
};

export default TableHeader;
