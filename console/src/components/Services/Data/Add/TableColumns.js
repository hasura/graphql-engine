import React from 'react';
import PropTypes from 'prop-types';
import TableColumn from './TableColumn';

const TableColumns = props => {
  const { columns } = props;
  const styles = require('../../../Common/TableCommon/Table.scss');
  const cols = columns.map((column, i) => {
    return (
      <TableColumn
        key={`table_column_wrapper_${i}`}
        colIndex={i}
        column={column}
        colLength={columns.length}
        {...props}
      />
    );
  });
  return [
    <h4 key="table_columns_header" className={styles.subheading_text}>
      Columns
    </h4>,
    <div key="table_colums_value">{cols}</div>,
  ];
};

TableColumns.propTypes = {
  columns: PropTypes.array.isRequired,
};

export default TableColumns;
