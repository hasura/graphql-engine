import React, { useMemo } from 'react';
import ReactTable from 'react-table';
import styles from '../../../Common/TableCommon/Table.scss';

interface ResultTableProps {
  headers: string[];
  rows: Array<string[]>;
}
const getTableConfig = (headers: string[], rows: Array<string[]>) => {
  const columns = headers.map(i => ({
    Header: i,
    accessor: i,
  }));
  const data = rows.map(i => {
    const result: Record<any, string> = {};
    headers.forEach((hdr, hIndex) => {
      result[hdr] = i[hIndex];
    });
    return result;
  });
  return { columns, data };
};

const ResultTable: React.FC<ResultTableProps> = ({ headers, rows }) => {
  console.log(headers, rows);
  const tableConfig = useMemo(() => getTableConfig(headers, rows), [
    headers,
    rows,
  ]);
  console.log(tableConfig);
  return (
    <div className={`${styles.addCol} col-xs-12 ${styles.padd_left_remove}`}>
      <h4 className={styles.subheading_text}>SQL Result:</h4>
      <ReactTable
        {...tableConfig}
        defaultPageSize={rows.length > 5 ? 5 : rows.length}
        className="-striped -highlight"
      />
      <br />
    </div>
  );
};

export default ResultTable;
