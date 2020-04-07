import React from 'react';
import styles from './PermissionStyles.scss';

const TableHeader = ({ headings }) => {
  return (
    <thead>
      <tr>
        {headings.map((heading, index) => {
          if (heading === '') {
            return (
              <th key={index} className={styles.bulkSelectCell}>
                {heading}
              </th>
            );
          }
          return <th key={index}>{heading}</th>;
        })}
      </tr>
    </thead>
  );
};

export default TableHeader;
