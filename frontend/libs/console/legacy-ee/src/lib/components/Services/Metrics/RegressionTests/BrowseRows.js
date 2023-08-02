import React, { useState, useEffect } from 'react';
import { useQuery } from '@apollo/react-hooks';
import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import FilterCheckboxComponent from '../StatsPanel/Filter/FilterCheckboxComponent';
import useSelectable from '../AllowLists/useSelectable';
import { ActionsPanel } from '../Common/ActionsPanel';
import { DeleteOperationsFromTests } from './DeleteOperationFromTests';
import { fetchOperationsForTestSuite } from './graphql.queries';
import styles from '../Metrics.module.scss';
import { PAGE_LIMIT, getTitle, getColWidth, DataNotAvaliable } from './utils';
import { InspectOperation } from './InspectOperation';
import { EditRow } from './EditRow';

/**
 * @typedef Props
 * @property {string} projectId
 * @property {string} testSuiteId
 *
 * @param {Props} props
 */
export const BrowseRows = props => {
  const { projectId, testSuiteId, changeToNewTab } = props;
  const [selectedNames, setSelectedNames] = useSelectable();
  const [offset, setOffset] = useState(0);
  const [pageLimit, setPageLimit] = useState(PAGE_LIMIT);

  const { loading, error, data, refetch } = useQuery(
    fetchOperationsForTestSuite,
    {
      variables: {
        projectId,
        testSuiteId,
        offset,
        limit: pageLimit,
      },
    }
  );

  useEffect(() => {
    refetch();
  }, [projectId]);

  const getCount = () => {
    return data.results_aggregate && data.results_aggregate.aggregate
      ? data.results_aggregate.aggregate.count
      : 0;
  };

  const getHeaders = () => {
    if (data.results.length > 0) {
      const columns = data.results[0];
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
          width: getColWidth(c, data.results),
        };
      });

      const onChangeAllSelection = () => {
        const operationNames = data.results.map(o => o.name) || [];
        if (selectedNames.length !== data.results.length) {
          setSelectedNames(operationNames);
        } else {
          setSelectedNames([]);
        }
      };
      const selectRowAction = {
        Header: (
          <div
            key="action_operation_header"
            className={`${styles.columnHeader} ${styles.textCenter}`}
          >
            <FilterCheckboxComponent
              title=""
              id="all"
              onChange={onChangeAllSelection}
              checked={selectedNames.length === data.results.length}
              bsClass={styles.bsCheckBoxClass}
            />
          </div>
        ),
        accessor: 'tableRowSelectAction',
        id: 'tableRowSelectAction',
        width: 80,
      };

      const actionRow = {
        Header: (
          <div key="action_operation_header" className={styles.columnHeader}>
            Actions
          </div>
        ),
        accessor: 'tableRowActionButtons',
        id: 'tableRowActionButtons',
        width: 100,
      };

      return [selectRowAction, actionRow, ...headerRows];
    }
    return [];
  };

  const getRows = () => {
    if (data.results.length > 0) {
      return data.results.map(row => {
        const newRow = {
          tableRowSelectAction: (
            <div className={styles.textCenter}>
              <FilterCheckboxComponent
                title=""
                id={row.name}
                onChange={setSelectedNames}
                checked={selectedNames.indexOf(row.name) !== -1}
                bsClass={styles.bsCheckBoxClass}
              />
            </div>
          ),
          tableRowActionButtons: (
            <div className={styles.textCenter}>
              <InspectOperation name={row.name} />
              <EditRow name={row.name} testSuiteId={testSuiteId} />
            </div>
          ),
        };
        Object.keys(row).forEach((elem, key) => {
          if (elem === 'session_variables' || elem === 'variables') {
            newRow[elem] = (
              <div
                key={key}
                className={`${styles.columnRow}`}
                title={JSON.stringify(row[elem])}
              >
                {JSON.stringify(row[elem])}
              </div>
            );
          } else {
            newRow[elem] = (
              <div
                key={key}
                className={`${styles.columnRow}`}
                title={row[elem]}
              >
                {row[elem]}
              </div>
            );
          }
        });
        return newRow;
      });
    }
    return [];
  };

  if (loading) {
    return <span>Getting operations from your Hasura account...</span>;
  }

  if (error) {
    return (
      <span className={styles.errorMessage}>
        Error fetching
        <code>{error.toString()}</code>
      </span>
    );
  }

  if (!data.results || data.results.length === 0) {
    return (
      <DataNotAvaliable>
        {' '}
        Add operations to your test suite by clicking{' '}
        <a href="#" onClick={changeToNewTab}>
          here
        </a>
      </DataNotAvaliable>
    );
  }

  const rows = getRows();
  const columns = getHeaders();

  return (
    <div
      className={`row ${tableScss.add_mar_top_small} col-xs-12  ${styles.addPaddBottom}`}
    >
      {rows.length ? (
        <div className={`${tableScss.tableContainer} ${styles.tableFullWidth}`}>
          <ActionsPanel>
            <DeleteOperationsFromTests
              operationNames={selectedNames}
              testSuiteId={testSuiteId}
              refetch={refetch}
            />
          </ActionsPanel>
          <DragFoldTable
            className="-highlight -fit-content"
            data={rows}
            columns={columns}
            manual
            showPagination
            sortable={false}
            minRows={0}
            pageSize={pageLimit}
            pages={Math.ceil(getCount() / pageLimit)}
            onPageChange={page => setOffset(page * pageLimit)}
            onPageSizeChange={setPageLimit}
            page={Math.floor(offset / pageLimit)}
          />
        </div>
      ) : (
        <DataNotAvaliable />
      )}
    </div>
  );
};
