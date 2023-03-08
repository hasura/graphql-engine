import React, { useState, useEffect } from 'react';
import { useQuery } from '@apollo/react-hooks';
import moment from 'moment';
import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import FilterCheckboxComponent from '../StatsPanel/Filter/FilterCheckboxComponent';
import useSelectable from '../AllowLists/useSelectable';
import { ActionsPanel } from '../Common/ActionsPanel';
import { fetchAllOperations } from './graphql.queries';
import { PAGE_LIMIT, getTitle, getColWidth } from './utils';
import { AddNewOperations } from './AddNewOperations';
import styles from '../Metrics.module.scss';
import { ReloadOperations } from './ReloadOperations';
import { InspectOperation } from './InspectOperation';

/**
 * @typedef Props
 * @property {boolean} showActionsPanel
 * @property {string} projectId
 * @property {string} projectName
 * @property {string} testSuiteId
 * @property {() => void} gotoTestSuite
 *
 * @param {Props} props
 */
export const BrowseAllOperations = props => {
  const { projectId, testSuiteId, gotoTestSuite, projectName } = props;

  const [selectedNames, setSelectedNames] = useSelectable();
  const [offset, setOffset] = useState(0);
  const [pageLimit, setPageLimit] = useState(PAGE_LIMIT);

  const { loading, error, data, refetch, networkStatus } = useQuery(
    fetchAllOperations,
    {
      variables: {
        projectId,
        offset,
        limit: pageLimit,
      },
      notifyOnNetworkStatusChange: true,
      fetchPolicy: 'network-only',
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
      const resultsRow = data.results[0];
      const columns = Object.keys(data.results[0]).reduce((acc, key) => {
        if (key === 'http_logs') {
          const httpLogsRow = resultsRow[key].length ? resultsRow[key][0] : {};
          return {
            ...acc,
            variables: JSON.stringify(httpLogsRow.variables),
            role: httpLogsRow.role,
            session_variables: JSON.stringify(httpLogsRow.session_variables),
          };
        }
        if (key === 'operation_id') return acc;
        return { ...acc, [key]: resultsRow[key] };
      }, {});

      const results = data.results.map(row => ({
        ...row,
        session_variables:
          row.http_logs.length && row.http_logs[0].session_variables,
        variables: row.http_logs.length && row.http_logs[0].variables,
        role: row.http_logs.length && row.http_logs[0].role,
      }));

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
          width: getColWidth(c, results),
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
      const results = data.results.map(row => ({
        ...row,
        session_variables:
          row.http_logs.length && row.http_logs[0].session_variables,
        variables: row.http_logs.length && row.http_logs[0].variables,
        role: row.http_logs.length && row.http_logs[0].role,
      }));
      delete results.http_logs;

      return results.map(row => {
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
            </div>
          ),
        };
        Object.keys(row).forEach((elem, key) => {
          let content;
          switch (elem) {
            case 'session_variables':
              content = JSON.stringify(row[elem]);
              break;
            case 'variables':
              content = JSON.stringify(row[elem] || {});
              break;
            case 'last_seen':
              content = moment(row[elem]).fromNow();
              break;
            default:
              content = row[elem];
          }
          newRow[elem] = (
            <div key={key} className={`${styles.columnRow}`} title={content}>
              {content}
            </div>
          );
        });
        return newRow;
      });
    }
    return [];
  };

  if (loading && !data) {
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

  const rows = data.results ? getRows() : [];
  const columns = data.results ? getHeaders() : [];

  return (
    <div
      className={`row ${tableScss.add_mar_top_small} col-xs-12  ${styles.addPaddBottom}`}
    >
      <ActionsPanel>
        <ReloadOperations
          projectName={projectName}
          networkStatus={networkStatus}
          refetch={refetch}
        />
        {selectedNames.length > 0 && (
          <AddNewOperations
            projectId={projectId}
            testSuiteId={testSuiteId}
            operations={data.results.filter(o =>
              selectedNames.includes(o.name)
            )}
            onComplete={() => {
              gotoTestSuite();
            }}
          />
        )}
      </ActionsPanel>
      {rows.length ? (
        <div className={`${tableScss.tableContainer} ${styles.tableFullWidth}`}>
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
        <span>
          There are no operations performed on this server yet or all the
          operations are already part of the test suite. Try reloading the list
          using the button above.
        </span>
      )}
      <div style={{ paddingTop: '10px' }}>
        * Subscriptions are not available on regression tests yet.
      </div>
    </div>
  );
};
