import React from 'react';
import PropTypes from 'prop-types';

import TableColumn from './TableColumn';
import { Heading } from '../../../UIKit/atoms';

const TableColumns = props => {
  const { columns } = props;

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
    <Heading key="table_columns_header" type="subHeading">
      Columns
    </Heading>,
    <div key="table_colums_value">{cols}</div>,
  ];
};

TableColumns.propTypes = {
  columns: PropTypes.array.isRequired,
};

export default TableColumns;
