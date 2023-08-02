import React from 'react';
import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import { getTitle, getColWidth, transformMessage, StatusIcon } from './utils';

import styles from '../Metrics.module.scss';

import { TestRunDetails } from './TestRunDetails';

export const BrowseTests = props => {
  const { testRunId, data } = props;
  const getHeaders = () => {
    if (data.test_run_details.length > 0) {
      const columns = data.test_run_details[0];
      const headerRows = Object.keys(columns).map((c, key) => {
        return {
          Header: (
            <div key={key} className={`${styles.columnHeader} ellipsis`}>
              {getTitle(c)}
            </div>
          ),
          accessor: c,
          id: c,
          foldable: true,
          width: getColWidth(c, data.test_run_details),
        };
      });

      const numberHeader = {
        Header: (
          <div key="number_header" className={styles.columnHeader}>
            No
          </div>
        ),
        accessor: 'operationNumber',
        id: 'operationNumber',
        width: 70,
      };

      const detailsHeader = {
        Header: (
          <div key="deyails_header" className={styles.columnHeader}>
            Details
          </div>
        ),
        accessor: 'details',
        id: 'details',
        width: 100,
      };

      return [numberHeader, detailsHeader, ...headerRows];
    }
    return [];
  };

  const getRows = () => {
    const rowData = data.test_run_details;
    if (rowData.length > 0) {
      return rowData.map((row, idx) => {
        const newRow = {
          operationNumber: (
            <div
              key={`opreration_number_${idx}`}
              className={`${styles.columnRow}`}
            >
              {idx + 1}
            </div>
          ),
          details: (
            <TestRunDetails
              status={row.status}
              testRunId={testRunId}
              operationName={row.name}
            />
          ),
        };
        Object.keys(row).forEach((elem, key) => {
          const renderElement = () => {
            if (elem === 'status') {
              return <StatusIcon status={row[elem]} />;
            } else if (elem === 'message') {
              return transformMessage(row);
            }
            return row[elem];
          };
          newRow[elem] = (
            <div
              key={key}
              className={`${styles.columnRow} ${
                elem === 'status' && styles.textCenter
              }`}
              title={
                (elem === 'status' && elem in row && row[elem]) ||
                renderElement()
              }
            >
              {renderElement()}
            </div>
          );
        });
        return newRow;
      });
    }
    return [];
  };

  const rows = data && data.test_run_details ? getRows() : [];
  const columns = data && data.test_run_details ? getHeaders() : [];

  const renderTable = () => {
    if (rows.length > 0) {
      return (
        <DragFoldTable
          className="-highlight -fit-content"
          data={rows}
          columns={columns}
          manual
          sortable={false}
          minRows={0}
          showPagination={false}
        />
      );
    }
    return (
      <span>
        There are no operations for this test run. Make sure there are
        operations added to the test suite.
      </span>
    );
  };

  return (
    <div className={`${tableScss.tableContainer} ${styles.tableFullWidth}`}>
      {renderTable()}
    </div>
  );
};
