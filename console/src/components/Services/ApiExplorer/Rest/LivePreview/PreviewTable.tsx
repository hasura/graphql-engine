import React from 'react';

import styles from '../RESTStyles.scss';

type PreviewTableProps = {
  headings: {
    content: string;
    className?: string;
  }[];
};

const PreviewTable: React.FC<PreviewTableProps> = ({ headings, children }) => (
  <table className={styles.rest_preview_table}>
    <tr className={styles.rest_preview_table_row}>
      {headings.map(heading => (
        <th className={heading?.className}>{heading.content}</th>
      ))}
    </tr>
    <tbody>{children}</tbody>
  </table>
);

export default PreviewTable;
