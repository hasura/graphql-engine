import React, { useMemo, ReactElement, ReactText } from 'react';
import ReactTable from 'react-table';
import { getColWidth } from '../../../Common/TableCommon/DragFoldTable';
import styles from '../../../Common/TableCommon/Table.scss';

interface ResultTableProps {
  headers: string[];
  rows: Array<string[]>;
}
const getColCellContent = (rowColumnValue: ReactText) => {
  let cellValue: React.ReactText | ReactElement = '';
  let cellTitle: React.ReactText | ReactElement = '';

  if (rowColumnValue === null) {
    cellValue = <i>NULL</i>;
    cellTitle = 'NULL';
  } else if (rowColumnValue === undefined) {
    cellValue = 'NULL';
    cellTitle = cellValue;
  } else if (typeof rowColumnValue === 'object') {
    cellValue = JSON.stringify(rowColumnValue, null, 4);
    cellTitle = cellValue;
  } else {
    cellValue = rowColumnValue.toString();
    cellTitle = cellValue;
  }

  return (
    <div className={styles.wd100} title={cellTitle}>
      {cellValue}
    </div>
  );
};

const getTableConfig = (headers: string[], rows: Array<string[]>) => {
  const dataMap: Record<string, any>[] = [];
  const data = rows.map(i => {
    const result: Record<string, any> = {};
    const resultMap: Record<string, any> = {};
    headers.forEach((hdr, hIndex) => {
      result[hdr] = getColCellContent(i[hIndex]);
      resultMap[hdr] = i[hIndex];
    });
    dataMap.push(resultMap);
    return result;
  });
  const columns = headers.map(i => ({
    Header: i,
    accessor: i,
    minWidth: Math.ceil(getColWidth(i, dataMap)),
  }));

  return { columns, data };
};

const ResultTable: React.FC<ResultTableProps> = ({ headers, rows }) => {
  const tableConfig = useMemo(() => getTableConfig(headers, rows), [
    headers,
    rows,
  ]);
  return (
    <div className={`${styles.addCol} col-xs-12 ${styles.padd_left_remove}`}>
      <h4 className={styles.subheading_text}>SQL Result:</h4>
      <div className={styles.tableContainer}>
        <ReactTable
          {...tableConfig}
          minRows={0} // if selected pagination is 100 and there are only 56 rows, hide empty rows
          defaultPageSize={rows.length > 10 ? 10 : rows.length}
          className="-striped -highlight result-table"
        />
      </div>
      <br />
    </div>
  );
};

export default ResultTable;
