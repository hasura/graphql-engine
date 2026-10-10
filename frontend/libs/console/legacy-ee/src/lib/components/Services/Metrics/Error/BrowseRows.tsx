import React, { useRef, useState } from 'react';
import { useQuery } from '@apollo/client/react';
import { Link } from 'react-router';
import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';
import { getFetchErrorsWithGroupBy } from './graphql.queries';
import {
  stripUnderScore,
  capitalize,
  getTimeRangeValue,
  getIfAliased,
  getActualIfAliased,
  createFilter,
  getWhereClauseEx,
  filterByType,
  checkIfEmptyReturnAppropriateValue,
} from './utils';

import {
  SHOW_ONLY_ERRORS_SYMBOL,
  relativeModulePath,
  TIME_RANGE_SYMBOL,
} from '../constants';

import {
  defaultColumns,
  customHeaders,
  PERCENT_OF_ERROR_SYMBOL,
  FILTER_MAP,
} from './constants';

import { FaCaretDown, FaCaretUp, FaSort } from 'react-icons/fa';

import styles from '../Metrics.module.scss';
import eye from '../images/usage.svg';
import { Flex } from '@radix-ui/themes';
import { Button } from '@hasura/shared/ui';

const LIMIT = 10;

const defaultState = {
  limit: LIMIT,
  offset: 0,
  order_by: {},
};

const BrowserRows = (props) => {
  const [browseState, setState] = useState(defaultState);
  const canvas = useRef(document.createElement('canvas'));
  const {
    filters: appliedFilters,
    groupBys,
    label,
    projectId,
    RenderLink,
  } = props;

  /* Get the list of filter types by filtering the type key of the filters list itself */

  // const whereClause = {};
  const getFilterObj = getWhereClauseEx(FILTER_MAP, appliedFilters);

  const { limit, offset, order_by } = browseState;

  const updateOrderByAndOffset = (l, o) => {
    setState({
      ...browseState,
      order_by: {
        [l.key]: l.value,
      },
      offset: o,
    });
  };

  const updateOffset = (o) => {
    setState({
      ...browseState,
      offset: o,
    });
  };

  const getGroupBys = () => {
    return groupBys.map((i) => i);
  };

  let fromTime = getTimeRangeValue();
  let now = new Date();
  now.setSeconds(0, 0);

  const timeRangeFilter = filterByType(appliedFilters, TIME_RANGE_SYMBOL);

  timeRangeFilter.forEach((e) => {
    if (typeof e.value === 'string') {
      fromTime = getTimeRangeValue(e.value);
    } else {
      const fDate = new Date(e.value.start);
      const toDate = new Date(e.value.end);
      fromTime = fDate.toISOString();
      now = toDate;
    }
  });

  const variables = {
    fromTime: fromTime,
    toTime: now.toISOString(),
    groupBys: getGroupBys(),
    orderBy: order_by,
    limit: limit,
    offset: offset,
    ...getFilterObj,
    project_id: [] as string[],
  };
  if (projectId) {
    variables.project_id = [projectId];
  }
  const { loading, error, data, refetch, networkStatus } = useQuery(
    getFetchErrorsWithGroupBy(),
    {
      variables: variables,
      notifyOnNetworkStatusChange: true,
    },
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

  const searchErrorMetrics = (data as any)?.searchErrorMetrics;

  const getHeaders = () => {
    if (searchErrorMetrics.length > 0) {
      const getColWidth = (header, contentRows = []) => {
        const MAX_WIDTH = 600;
        const HEADER_PADDING = 62;
        const CONTENT_PADDING = 36;
        const HEADER_FONT = 'bold 16px Gudea';
        const CONTENT_FONT = '14px Gudea';

        const getTextWidth = (text, font) => {
          // Doesn't work well with non-monospace fonts
          // const CHAR_WIDTH = 8;
          // return text.length * CHAR_WIDTH;

          // if given, use cached canvas for better performance
          // else, create new canvas

          const context = canvas.current.getContext('2d');
          if (!context) {
            return 0;
          }

          context.font = font;

          const metrics = context.measureText(text);
          return metrics.width;
        };

        let maxContentWidth = 0;
        for (let i = 0; i < contentRows.length; i++) {
          if (contentRows[i] !== undefined && contentRows[i][header] !== null) {
            const content = contentRows[i][header];

            let contentString;
            if (content === null || content === undefined) {
              contentString = 'NULL';
            } else if (typeof content === 'object') {
              contentString = JSON.stringify(content, null, 4);
            } else {
              contentString = String(content);
            }

            const currLength = getTextWidth(contentString, CONTENT_FONT);

            if (currLength > maxContentWidth) {
              maxContentWidth = currLength;
            }
          }
        }

        const maxContentCellWidth = maxContentWidth + CONTENT_PADDING + 12;

        const headerCellWidth =
          getTextWidth(getIfAliased(header), HEADER_FONT) + HEADER_PADDING;

        return Math.min(
          MAX_WIDTH,
          Math.max(maxContentCellWidth, headerCellWidth),
        );
      };
      let validColumns = [...defaultColumns];
      const haveExtraColumns = variables.groupBys;
      if (haveExtraColumns.length > 0) {
        const newColumns = haveExtraColumns;
        validColumns = [...newColumns, ...defaultColumns];
      }
      const columns = searchErrorMetrics[0];
      const headerRows = Object.keys(columns)
        .filter((f) => validColumns.indexOf(getIfAliased(f)) !== -1)
        .map((c, key) => {
          let sortIcon: React.ReactNode = <FaSort />;
          if (order_by && Object.keys(order_by).length) {
            sortIcon = '';
            const col = getIfAliased(c);
            if (col in order_by) {
              sortIcon =
                order_by[col] === 'asc' ? <FaCaretUp /> : <FaCaretDown />;
            }
          }
          return {
            header: () => (
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
            accessorKey: c,
            cell: (info) => info.getValue(),
            id: c,
            foldable: true,
            size: getColWidth(c, searchErrorMetrics),
          };
        });

      customHeaders.forEach((i) => {
        const customHeader = {
          header: () => (
            <div key={i} className={`${styles.columnHeader} ellipsis`}>
              <span className={tableScss.tableHeaderCell}>
                {capitalize(stripUnderScore(i))}
              </span>
            </div>
          ),
          accessorKey: i,
          cell: (info) => info.getValue(),
          id: i,
          foldable: true,
          size: getColWidth(i, searchErrorMetrics),
        };
        headerRows.push(customHeader as any);
      });

      const actionRow = {
        header: () => (
          <div
            key={'action_operation_header'}
            className={`${styles.columnHeader}`}
          >
            Actions
          </div>
        ),
        accessorKey: 'tableRowActionButtons',
        cell: (info) => info.getValue(),
        id: 'tableRowActionButtons',
        size: 85,
      };
      return [actionRow, ...headerRows];
      /* return headerRows; */
    }
    return [];
  };

  const getRows = () => {
    if (searchErrorMetrics?.length > 0) {
      return searchErrorMetrics.map((d) => {
        const newRow: Record<string, any> = {};
        const getLinkToOperationsPage = () => {
          /* Get the groupBys */
          if (groupBys.length > 0) {
            const haveExtraColumns = variables.groupBys;
            const newColumns = haveExtraColumns;
            const filters = newColumns.map((n) => {
              const getValue = () => {
                let valLen = 0;
                if (d[getActualIfAliased(n)]) {
                  valLen = d[getActualIfAliased(n)].length;
                }
                /* Return appropriate
                 * values so as to link to operations
                 * with right filters applied
                 * */
                if (valLen === 0) {
                  return checkIfEmptyReturnAppropriateValue(n);
                }
                return d[getActualIfAliased(n)];
              };
              const val = getValue();
              return createFilter(n, val);
            });
            /* Show only errors on the operations page */
            filters.push({
              type: SHOW_ONLY_ERRORS_SYMBOL,
              value: SHOW_ONLY_ERRORS_SYMBOL,
            });

            const searchParam = `?filters=${window.encodeURI(
              JSON.stringify(filters),
            )}`;

            const lensIcon = (
              <img className={styles.actionImg} src={eye} alt={'Inspect row'} />
            );

            if (RenderLink) {
              return (
                <RenderLink pageName="operations" search={searchParam}>
                  {lensIcon}
                </RenderLink>
              );
            }
            return (
              <Link
                to={{
                  pathname: `${relativeModulePath}/operations`,
                  search: searchParam,
                }}
              >
                {lensIcon}
              </Link>
            );
          }
          return null;
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
          newRow[elem] = (
            <div key={key} className={styles.columnRow} title={d[elem]}>
              {d[elem]}
            </div>
          );
        });
        /* Adding custom headers */
        customHeaders.forEach((i) => {
          if (i === PERCENT_OF_ERROR_SYMBOL) {
            let val = 'N/A';
            if ('error_count' in d && 'request_count' in d) {
              if (d.request_count > 0) {
                val = `${Math.round(
                  (d.error_count / d.request_count) * 100,
                )} %`;
              }
            }
            newRow[PERCENT_OF_ERROR_SYMBOL] = (
              <div key={i} className={styles.columnRow} title={val}>
                {val}
              </div>
            );
          }
        });
        return newRow;
      });
    }
    return [];
  };

  const getCount = () => {
    const searchErrorMetricsAggregate = (data as any)
      .searchErrorMetricsAggregate;
    if ('count' in searchErrorMetricsAggregate) {
      return searchErrorMetricsAggregate.count;
    }
    return 0;
  };

  const _rows = getRows();
  const _columns = getHeaders();

  const handlePageChange = (page) => {
    if (offset !== page * limit) {
      updateOffset(page * limit);
    }
  };

  const sortByColumn = (currColumn) => {
    if (!searchErrorMetrics?.length) {
      console.error('Minimum one row required to sort');
      return;
    }
    const rowEntry = searchErrorMetrics[0];
    const columnNames = Object.keys(rowEntry).map((column) => column);

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
        0,
      );
    } else {
      updateOrderByAndOffset(
        {
          key: orderByCol,
          value: orderType,
        },
        0,
      );
    }
  };

  const renderPagination = () => {
    const currentPage = Math.floor(offset / limit);
    const totalPages = Math.ceil(getCount() / limit);

    return (
      <div className="flex ml-2 mr-2 mb-2 mt-2 justify-around max-w-5xl">
        <Button
          color="gray"
          onClick={() => handlePageChange(currentPage - 1)}
          disabled={currentPage <= 0}
          data-test="view-rows-pagination-prev"
        >
          Prev
        </Button>
        <div>
          Page {currentPage + 1} of {totalPages || 1}
        </div>
        <Button
          color="gray"
          onClick={() => handlePageChange(currentPage + 1)}
          disabled={currentPage + 1 >= totalPages}
          data-test="view-rows-pagination-next"
        >
          Next
        </Button>
      </div>
    );
  };

  const renderErrorTable = () => {
    return (
      <div className={styles.clearBoth + ' ' + styles.addPaddTop}>
        <Flex align="center" justify="center" className={styles.addPaddBottom}>
          <div className={styles.subHeader}>{label}</div>
          <div className="ml-4">
            <Button color="gray" onClick={() => refetch()}>
              {refetchRender()}
            </Button>
          </div>
        </Flex>
        <div className={tableScss.tableContainer}>
          {_rows.length > 0 ? (
            <DragFoldTable
              data={_rows}
              columns={_columns}
              onSort={sortByColumn}
              renderPagination={renderPagination}
            />
          ) : (
            <div>No results found!</div>
          )}
        </div>
      </div>
    );
  };

  return (
    <div className={`row mt-6`}>
      <div className="col-xs-12">{renderErrorTable()}</div>
    </div>
  );
};

export default BrowserRows;
