import React, { useEffect, useState } from 'react';

import moment from 'moment';
import { Button } from '@hasura/console-legacy-ce';
import { Tooltip } from '@hasura/console-legacy-ce';

import { useQuery } from '@apollo/react-hooks';
import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import { arrayToPostgresArray, isAdmin } from '../utils';
import { getConfig } from './Actions';
import {
  DEFAULT_ORDER_BY,
  defaultColumns,
  defaultHeaders,
  EXECUTION_TIME_DIVIDER_CONSTANT,
  FILTER_MAP,
} from './constants';
import { ErrorModal } from './ErrorModal';
import { getSubscriptionWorkers } from './graphql.queries';
import Inspect from './Inspect';
// import Modal from './Modal';
import {
  arraysEqual,
  capitalize,
  getColWidth,
  getIfAliased,
  getWhereClause,
  roundToTwo,
  sortHeaders,
  stripUnderScore,
} from './utils';
import { FaCaretDown, FaCaretUp, FaSort } from 'react-icons/fa';

import styles from '../Metrics.module.scss';
import inspectRow from '../images/warningNew.svg';
import graphIcon from '../images/graph.svg';

const LIMIT = 10;

const defaultState = {
  limit: LIMIT,
  offset: 0,
  order_by: { ...DEFAULT_ORDER_BY },
};

// deserialize poller_log data to subscription worker
const deserializeSubscriptionWorker = worker => {
  const operationLog =
    worker?.operation_logs?.length === 0 ? {} : worker.operation_logs[0];

  return {
    ...worker,
    ...operationLog,
  };
};

const BrowseRows = props => {
  const [browseState, setState] = useState(defaultState);
  const [config, setConfig] = useState(null);
  const [selectedRow, updateSelectedRow] = useState(null);
  const [configError, setConfigError] = useState(null);
  const [showModal, updateModal] = useState(false);
  const {
    filters: appliedFilters,
    label,
    projectId,
    dispatch,
    updateRowData,
    privileges,
  } = props;

  const getFilterObj = getWhereClause(FILTER_MAP, appliedFilters);

  useEffect(() => {
    if (isAdmin(privileges)) {
      dispatch(getConfig())
        .then(({ live_queries }) => {
          setConfig(live_queries);
        })
        .catch(err => {
          setConfigError(JSON.stringify(err, null, 4));
          updateModal(true);
        });
    }
  }, []);

  const onRowClick = row => {
    if (selectedRow) {
      if (selectedRow.workerId === row.worker_id) {
        updateSelectedRow(null);
        updateRowData(null);
      } else {
        updateSelectedRow({
          workerId: row.worker_id,
        });
        updateRowData(row);
      }
    } else {
      updateSelectedRow({
        workerId: row.worker_id,
      });
      updateRowData(row);
    }
  };

  const getRefetchDelay = () => {
    if (config) {
      return config.refetch_delay;
    }
    return null;
  };

  const getBatchSize = () => {
    if (config) {
      return config.batch_size;
    }
    return null;
  };

  const { limit, offset, order_by } = browseState;

  const argsClause = {
    project_ids: projectId ? `{${projectId}}` : '{}',
    poller_ids: arrayToPostgresArray(getFilterObj.poller_ids),
    from_time: getFilterObj.from_time,
    to_time: getFilterObj.to_time,
    status: arrayToPostgresArray(getFilterObj.status),
  };

  const whereClause =
    !getFilterObj.operation_name || !getFilterObj.operation_name.length
      ? {}
      : {
          operation_logs: {
            operation_name: {
              _in: [...getFilterObj.operation_name],
            },
          },
        };
  const updateLimit = l => {
    setState({
      ...browseState,
      limit: l,
    });
  };

  const updateOrderByAndOffset = (l, o) => {
    setState({
      ...browseState,
      order_by: {
        [l.key]: l.value,
      },
      offset: o,
    });
  };

  const updateOffset = o => {
    setState({
      ...browseState,
      offset: o,
    });
  };

  const variables = {
    limit: limit,
    offset: offset,
    args: argsClause,
    where: whereClause,
    orderBy: {
      ...order_by,
    },
  };

  const { loading, error, data, refetch, networkStatus } = useQuery(
    getSubscriptionWorkers(),
    {
      variables: variables,
      notifyOnNetworkStatusChange: true,
    }
  );

  const subscriptionWorkers =
    data &&
    data.search_subscription_workers &&
    data.search_subscription_workers.length > 0
      ? data.search_subscription_workers.map(deserializeSubscriptionWorker)
      : [];

  if (loading) {
    return 'Loading ...';
  }
  if (error) {
    return 'Error fetching';
  }

  const refetchRender = () => {
    if (networkStatus === 4) {
      return 'Reloading ...';
    }
    return 'Reload';
  };

  const columns =
    subscriptionWorkers.length <= 0
      ? []
      : Object.keys(subscriptionWorkers[0]).filter(
          f => defaultColumns.indexOf(getIfAliased(f)) !== -1
        );

  const getHeaders = () => {
    if (subscriptionWorkers.length > 0) {
      const headers = columns.map((c, key) => {
        let sortIcon = <FaSort />;
        if (order_by && Object.keys(order_by).length) {
          sortIcon = '';
          const col = getIfAliased(c);
          if (col in order_by) {
            sortIcon =
              order_by[col] === 'asc' ? <FaCaretUp /> : <FaCaretDown />;
          }
        }
        return {
          Header: (
            <div
              key={key}
              className={`${styles.columnHeader} ellipsis`}
              title="Click to sort"
            >
              <span className={tableScss.tableHeaderCell}>
                {capitalize(stripUnderScore(c))}
                <span className={tableScss.tableHeaderCell}>{sortIcon}</span>
              </span>
            </div>
          ),
          accessor: c,
          id: c,
          foldable: true,
          width: getColWidth(c, subscriptionWorkers),
        };
      });

      const headerRows = sortHeaders(headers, defaultHeaders);

      const actionRow = {
        Header: (
          <div
            key={'action_operation_header'}
            className={`${styles.columnHeader}`}
          >
            Actions
          </div>
        ),
        accessor: 'tableRowActionButtons',
        id: 'tableRowActionButtons',
        width: 85,
      };
      return [actionRow, ...headerRows];
      /* return headerRows; */
    }
    return [];
  };

  const renderActionButtons = r => {
    return (
      <div className={styles.textCenter}>
        <Inspect projectId={projectId} pollerId={r.worker_id} />
        <div
          onClick={() => {
            onRowClick(r);
          }}
        >
          <img className={styles.actionImg} src={graphIcon} alt="" />
        </div>
        {/* renderActionButtonForGroups() */}
      </div>
    );
  };

  const renderWarningSymbol = execTime => {
    if (
      getRefetchDelay() &&
      execTime >= getRefetchDelay() / EXECUTION_TIME_DIVIDER_CONSTANT
    ) {
      return (
        <div className={styles.paddingLeft}>
          <Tooltip
            side="right"
            tooltipContentChildren="This subscription is taking as much time as the refetch delay to execute. You might want to adjust the batch size or adjust the delay to accomodate the execution time. You can also try optimising the SQL query by adding the right indexes."
          >
            <img
              className={styles.actionImg}
              src={inspectRow}
              alt={'Inspect row'}
            />
          </Tooltip>
        </div>
      );
    }
    return null;
  };

  const getRows = () => {
    if (subscriptionWorkers.length > 0) {
      return subscriptionWorkers.map((d, y) => {
        return columns.reduce(
          (acc, col, x) => {
            const key = `subscription-worker-${col}-${y}-${x}`;

            if (d[col] === null || d[col] === undefined) {
              return {
                ...acc,
                [col]: (
                  <div key={key} className={styles.columnRow}>
                    --
                  </div>
                ),
              };
            }

            let cell = null;

            switch (col) {
              case 'conn_err':
                cell = <div className={styles.textCenter}>{null}</div>;
                break;
              case 'started':
                cell = (
                  <div key={key} className={styles.columnRow} title={d[col]}>
                    {moment(new Date(d[col])).fromNow()}
                  </div>
                );
                break;
              case 'status':
                cell = (
                  <div
                    key={key}
                    className={styles.columnRow + ' ' + styles.flexRow}
                    title={d[col]}
                  >
                    <div>{d[col]}</div>
                    {renderWarningSymbol(d.last_execution_time)}
                  </div>
                );
                break;

              case 'last_execution_time':
                cell = (
                  <div key={key} className={styles.columnRow} title={d[col]}>
                    {`${roundToTwo(d[col] * 1000)} ms`}
                  </div>
                );
                break;

              case 'session_variables':
              case 'query_variables':
                if (arraysEqual(Object.keys(d[col] || {}), [])) {
                  cell = (
                    <div key={key} className={styles.columnRow} title={d[col]}>
                      --
                    </div>
                  );
                } else {
                  const keys = Object.keys(d[col]);
                  const separator = ',';
                  cell = (
                    <Tooltip
                      side="bottom"
                      tooltipContentChildren={JSON.stringify(d[col])}
                    >
                      <div
                        key={key}
                        className={styles.columnRow}
                        title={d[col]}
                      >
                        {keys.map((p, i) => {
                          if (i !== keys.length - 1 && p !== 'x-hasura-role') {
                            return `${p}${separator} `;
                          } else if (
                            p === 'x-hasura-role' &&
                            keys.length === 1
                          ) {
                            return '--';
                          } else if (p === 'x-hasura-role') {
                            return null;
                          }
                          return p;
                        })}
                      </div>
                    </Tooltip>
                  );
                }
                break;
              default:
                cell = (
                  <div key={key} className={styles.columnRow} title={d[col]}>
                    {d[col]}
                  </div>
                );
            }

            return {
              ...acc,
              [col]: cell,
            };
          },
          {
            tableRowActionButtons: renderActionButtons(d),
          }
        );
      });
    }

    return [];
  };

  const getCount = () => {
    if ('aggregate' in data.search_subscription_workers_aggregate) {
      return data.search_subscription_workers_aggregate.aggregate.count;
    }
    return 0;
  };

  const _rows = getRows();
  const _columns = getHeaders();

  const handlePageChange = page => {
    if (offset !== page * limit) {
      updateOffset(page * limit);
      updateSelectedRow(null);
      updateRowData(null);
    }
  };

  const handlePageSizeChange = size => {
    if (limit !== size) {
      updateLimit(size);
    }
  };

  let disableSortColumn = false;

  const sortByColumn = currColumn => {
    if (subscriptionWorkers.length === 0) {
      console.error('Minimum one row required to sort');
      return;
    }
    const rowEntry = subscriptionWorkers[0];
    const columnNames = Object.keys(rowEntry).map(column => column);

    if (!columnNames.includes(currColumn)) {
      return;
    }

    const col = getIfAliased(currColumn);
    const orderByCol = col;
    const orderType = 'asc';

    /* There is going to be only one order by clause */

    if (orderByCol in order_by && order_by[orderByCol] === 'asc') {
      updateOrderByAndOffset(
        {
          key: orderByCol,
          value: 'desc',
        },
        0
      );
    } else {
      updateOrderByAndOffset(
        {
          key: orderByCol,
          value: orderType,
        },
        0
      );
    }
  };

  const getTheadThProps = (finalState, some, column) => ({
    onClick: () => {
      if (!disableSortColumn && column.id) {
        sortByColumn(column.id);
      }

      disableSortColumn = false;
    },
  });

  const getResizerProps = (finalState, none, column, ctx) => ({
    onMouseDown: e => {
      disableSortColumn = true;
      ctx.resizeColumnStart(e, column, false);
    },
  });

  const getTrProps = (state, rowInfo) => {
    if (rowInfo) {
      if (
        getRefetchDelay() &&
        rowInfo.row.last_execution_time.props.title >=
          getRefetchDelay() / EXECUTION_TIME_DIVIDER_CONSTANT
      ) {
        return {
          style: {
            background: '#FBE192',
          },
        };
      }
      if (selectedRow) {
        if (rowInfo.row.worker_id.props.title === selectedRow.workerId) {
          return {
            style: {
              background: '#EBF8DE',
            },
          };
        }
      }

      return {};
    }
    return {};
  };

  const renderSubscriptionWorkersTable = () => {
    return (
      <div className={styles.clearBoth + ' ' + styles.addPaddTop}>
        <div className={styles.tableTopContent}>
          <div className={styles.flexRow}>
            <div className={styles.subHeader}>{label}</div>
            <div className="ml-4">
              <Button
                onClick={() => {
                  refetch();
                  updateSelectedRow(null);
                  updateRowData(null);
                }}
              >
                {refetchRender()}
              </Button>
            </div>
          </div>
          <div className={styles.flexColumn}>
            <div>
              Batch Size:
              <span
                className={styles.fontWeightBold + ' ' + styles.paddingLeft}
              >
                {getBatchSize()}
              </span>
            </div>
            <div>
              Refetch delay:
              <span
                className={styles.fontWeightBold + ' ' + styles.paddingLeft}
              >
                {`${getRefetchDelay()}s`}
              </span>
            </div>
          </div>
        </div>
        <div className={tableScss.tableContainer}>
          {_rows.length > 0 ? (
            <DragFoldTable
              className="-highlight -fit-content"
              data={_rows}
              columns={_columns}
              resizable
              manual
              getTheadThProps={getTheadThProps}
              getResizerProps={getResizerProps}
              sortable={false}
              minRows={0}
              pageSize={limit}
              pages={Math.ceil(getCount() / limit)}
              onPageChange={handlePageChange}
              onPageSizeChange={handlePageSizeChange}
              page={Math.floor(offset / limit)}
              getTrProps={getTrProps}
            />
          ) : (
            <div>No results found!</div>
          )}
        </div>
      </div>
    );
  };

  return (
    <div className={`row ${tableScss.add_mar_top}`}>
      <div className="col-xs-12">{renderSubscriptionWorkersTable()}</div>
      <ErrorModal
        show={showModal}
        onHide={() => updateModal(false)}
        err={configError}
      />
    </div>
  );
};

export default BrowseRows;
