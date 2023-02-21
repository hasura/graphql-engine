import React, { useState } from 'react';
import { useQuery } from '@apollo/react-hooks';
import { Button } from '@hasura/console-legacy-ce';
import { Tooltip } from '@hasura/console-legacy-ce';
import { Link } from 'react-router';

import { getFetchWebsocketsStatus } from './graphql.queries';
import {
  stripUnderScore,
  capitalize,
  parseGroupByArrayType,
  getIfAliased,
  getColWidth,
} from './utils';

import { getWhereClause } from './utils';
import { createFilter } from '../Error/utils';

import { relativeModulePath, WEBSOCKET_ID_SYMBOL } from '../constants';

import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import { defaultColumns, FILTER_MAP, DEFAULT_ORDER_BY } from './constants';
import moment from 'moment';
import { FaCaretDown, FaCaretUp, FaSort } from 'react-icons/fa';

import styles from '../Metrics.module.scss';
import eye from '../images/usage.svg';
import failure from '../images/failure.svg';
import success from '../images/success.svg';

const LIMIT = 10;

const defaultState = {
  limit: LIMIT,
  offset: 0,
  order_by: { ...DEFAULT_ORDER_BY },
};

const BrowseRows = props => {
  const [browseState, setState] = useState(defaultState);
  const { filters: appliedFilters, label, projectId } = props;

  const { limit, offset, order_by } = browseState;

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

  const getFilterObj = getWhereClause(FILTER_MAP, appliedFilters);

  const whereClause = {
    _and: [...getFilterObj],
  };

  if (projectId) {
    /* Filter by project id */
    whereClause.project_id = {
      _eq: projectId,
    };
  }

  const variables = {
    limit: limit,
    offset: offset,
    where: whereClause,
    orderBy: order_by,
  };

  const { loading, error, data, refetch, networkStatus } = useQuery(
    getFetchWebsocketsStatus(variables.groupBys),
    {
      variables: variables,
      notifyOnNetworkStatusChange: true,
    }
  );
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

  const getHeaders = () => {
    if (data && data.websocket_status.length > 0) {
      let validColumns = [...defaultColumns];
      const haveExtraColumns = parseGroupByArrayType(variables.groupBys);
      if (haveExtraColumns) {
        if (haveExtraColumns[1].length > 0) {
          const newColumns = haveExtraColumns[1].split(',');
          validColumns = [...newColumns, ...defaultColumns];
        }
      }
      const columns = {
        ...data.websocket_status[0],
        ...data.websocket_status[0],
      };
      const headerRows = Object.keys(columns)
        .filter(f => validColumns.indexOf(getIfAliased(f)) !== -1)
        .map((c, key) => {
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
            width: getColWidth(c, data.websocket_status),
          };
        });

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

  const getRows = () => {
    if (data && data.websocket_status.length > 0) {
      return data.websocket_status.map(d => {
        const newRow = {};
        const getLinkToOperationsPage = () => {
          /* Get the groupBys */
          const lensIcon = (
            <img className={styles.actionImg} src={eye} alt={'Inspect row'} />
          );
          const filters = [createFilter(WEBSOCKET_ID_SYMBOL, d.websocket_id)];
          return (
            <Tooltip side="right" tooltipContentChildren="View operations">
              <Link
                to={{
                  pathname: `${relativeModulePath}/operations`,
                  search: `?filters=${window.encodeURI(
                    JSON.stringify(filters)
                  )}`,
                }}
              >
                {lensIcon}
              </Link>
            </Tooltip>
          );
        };
        newRow.tableRowActionButtons = (
          <div className={styles.textCenter}>
            {getLinkToOperationsPage()}
            {/*
            <Modal />
            */}
          </div>
        );
        Object.keys(d).forEach((elem, key) => {
          if (d[elem] === 'conn_err') {
            newRow.tableRowActionButtons = (
              <div className={styles.textCenter}>{null}</div>
            );
          }
          if (elem === 'time') {
            newRow[elem] = (
              <div key={key} className={styles.columnRow} title={d[elem]}>
                {moment(new Date(d[elem])).fromNow()}
              </div>
            );
          } else if (elem === 'success') {
            if (!d[elem]) {
              newRow[elem] = (
                <div key={key} className={styles.columnRow} title={d[elem]}>
                  <img src={success} alt={'Success'} />
                </div>
              );
            } else {
              newRow[elem] = (
                <div key={key} className={styles.columnRow} title={d[elem]}>
                  <img src={failure} alt={'Failure'} />
                </div>
              );
            }
          } else {
            newRow[elem] = (
              <div key={key} className={styles.columnRow} title={d[elem]}>
                {d[elem]}
              </div>
            );
          }
        });
        /* Adding custom headers */
        return newRow;
      });
    }
    return [];
  };

  const getCount = () => {
    if ('aggregate' in data.websocket_status_aggregate) {
      return data.websocket_status_aggregate.aggregate.count;
    }
    return 0;
  };

  const _rows = getRows();
  const _columns = getHeaders();

  const handlePageChange = page => {
    if (offset !== page * limit) {
      updateOffset(page * limit);
    }
  };

  const handlePageSizeChange = size => {
    if (limit !== size) {
      updateLimit(size);
    }
  };

  let disableSortColumn = false;

  const sortByColumn = currColumn => {
    if (data && data.websocket_status.length === 0) {
      console.error('Minimum one row required to sort');
      return;
    }
    const rowEntry = data.websocket_status[0];
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

  const renderWebsocketsTable = () => {
    return (
      <div className={styles.clearBoth + ' ' + styles.addPaddTop}>
        <div className={styles.displayFlex + ' ' + styles.addPaddBottom}>
          <div className={styles.subHeader}>{label}</div>
          <div className="ml-4">
            <Button onClick={() => refetch()}>{refetchRender()}</Button>
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
      <div className="col-xs-12">{renderWebsocketsTable()}</div>
    </div>
  );
};

export default BrowseRows;
