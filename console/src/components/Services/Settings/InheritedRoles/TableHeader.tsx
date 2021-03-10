import React from 'react';
import styles from './InheritedRolesStyles.scss';

type TableHeaderProps = {
  headings: string[];
};

const TableHeader: React.FC<TableHeaderProps> = props => {
  const { headings } = props;
  return (
    <thead>
      <tr>
        {headings.map((heading, index) =>
          heading === 'Inherited Role' ? (
            <th key={index} className={styles.fix_column_width}>
              {heading}
            </th>
          ) : (
            <th key={index}>{heading}</th>
          )
        )}
      </tr>
    </thead>
  );
};

export default TableHeader;
