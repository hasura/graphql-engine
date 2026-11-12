import React from 'react';
import { Skeleton } from '@radix-ui/themes';
import { Table } from '@hasura/shared/ui';

type Props = {
  columnCount: number;
  rowCount?: number;
};

/**
 * Placeholder rows shown in the table body while its rows are loading.
 */
const TableLoadingRows: React.FC<Props> = ({ columnCount, rowCount = 5 }) => (
  <>
    {Array.from({ length: rowCount }, (_, rowIndex) => (
      <Table.Row key={rowIndex} data-testid="table-loading-row">
        {Array.from({ length: columnCount }, (__, columnIndex) => (
          <Table.Cell key={columnIndex}>
            <Skeleton height="16px" />
          </Table.Cell>
        ))}
      </Table.Row>
    ))}
  </>
);

export default TableLoadingRows;
