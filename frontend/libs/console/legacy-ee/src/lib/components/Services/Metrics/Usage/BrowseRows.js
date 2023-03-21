import React, { useState } from 'react';

import { useQuery } from '@apollo/react-hooks';
import { Button } from '@hasura/console-legacy-ce';

import { fetchQueryList } from './graphql.queries';
import Inspect from './Inspect';
// import Inspect from './Inspect';
import {
  stripUnderScore,
  capitalize,
  filterByType,
  getTimeRangeValue,
  transformedVals,
  transformedHeaderVal,
  getIfAliased,
} from './utils';

import { getWhereClauseEx } from '../Error/utils';

import { FILTER_MAP } from './constants';

import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import { TIME_RANGE_SYMBOL } from '../constants';

import { aliasedNames, headerTitleLabel, defaultColumns } from './constants';
import { FaCaretDown, FaCaretUp, FaSort } from 'react-icons/fa';

import styles from '../Metrics.module.scss';
import failure from '../images/failure.svg';
import success from '../images/success.svg';

const LIMIT = 10;

const BrowserRows = props => {
  const defaultState = {
    limit: LIMIT,
    offset: 0,
    order_by: {
      operation_name: 'asc',
    },
    now: new Date().toISOString(),
  };

  const [browseState, setState] = useState(defaultState);
  const { filters, groupBys, label, projectId, RenderLink } = props;

  /* Get the list of filter types by filtering the type key of the filters list itself */

  const getFilterObj = getWhereClauseEx(FILTER_MAP, filters);

  const getGroupBys = () => {
    const groupBy = groupBys.map(i => {
      return i;
    });
    return groupBy;
  };

  const whereClause = {
    fromTime: getTimeRangeValue(),
    toTime: browseState.now,
    groupBys: [...getGroupBys()],
  };
  const timeRangeFilter = filterByType(filters, TIME_RANGE_SYMBOL);

  timeRangeFilter.forEach(e => {
    if (typeof e.value === 'string') {
      whereClause.fromTime = getTimeRangeValue(e.value);
    } else {
      const fDate = new Date(e.value.start);
      const toDate = new Date(e.value.end);
      whereClause.fromTime = fDate.toISOString();
      whereClause.toTime = toDate.toISOString();
    }
  });

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

  const variables = {
    limit: limit,
    offset: offset,
    ...whereClause,
    groupBys: whereClause.groupBys,
    ...getFilterObj,
    orderBy: order_by,
    project_id: [],
  };
  if (projectId) {
    variables.project_id = [projectId];
  }
  const { loading, error, data, refetch, networkStatus } = useQuery(
    fetchQueryList,
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
    if (data.searchUsageMetrics.length > 0) {
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
          const canvas =
            getTextWidth.canvas ||
            (getTextWidth.canvas = document.createElement('canvas'));

          const context = canvas.getContext('2d');
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
              if (header in transformedVals) {
                contentString = transformedVals[header](content);
              } else {
                contentString = content.toString();
              }
            }

            const currLength = getTextWidth(contentString, CONTENT_FONT);

            if (currLength > maxContentWidth) {
              maxContentWidth = currLength;
            }
          }
        }

        const maxContentCellWidth = maxContentWidth + CONTENT_PADDING + 12;

        const headerWithUnit = h => {
          if (h in transformedHeaderVal) {
            return transformedHeaderVal[h](h);
          }
          return h;
        };

        const headerCellWidth =
          getTextWidth(headerWithUnit(getIfAliased(header)), HEADER_FONT) +
          HEADER_PADDING;
        return Math.min(
          MAX_WIDTH,
          Math.max(maxContentCellWidth, headerCellWidth)
        );
      };
      let validColumns = [...defaultColumns];
      if (whereClause.groupBys.length > 0) {
        validColumns = [...whereClause.groupBys, ...defaultColumns];
      }
      const columns = data.searchUsageMetrics[0];
      const headerRows = Object.keys(columns)
        .filter(f => validColumns.indexOf(getIfAliased(f)) !== -1)
        .map((c, key) => {
          let sortIcon = <FaSort />;
          if (order_by && Object.keys(order_by).length) {
            sortIcon = '';
            const col = c in aliasedNames ? aliasedNames[c] : c;

            if (col in order_by) {
              sortIcon =
                order_by[col] === 'asc' ? <FaCaretUp /> : <FaCaretDown />;
            }
          }
          const unitIfThereIs = () => {
            if (c in headerTitleLabel) {
              return `(${headerTitleLabel[c]})`;
            }
            return '';
          };

          const getWithUnitHeaderTitle = () => {
            return `${capitalize(stripUnderScore(c))}${unitIfThereIs()}${' '}`;
          };
          return {
            Header: (
              <div
                key={key}
                className={`${styles.columnHeader} ellipsis`}
                title="Click to sort"
              >
                {getWithUnitHeaderTitle()}
                <span className={tableScss.tableHeaderCell}>{sortIcon}</span>
              </div>
            ),
            accessor: c,
            id: c,
            foldable: true,
            width: getColWidth(c, data.searchUsageMetrics),
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
    }
    return [];
  };

  const getRows = () => {
    if (data.searchUsageMetrics.length > 0) {
      return data.searchUsageMetrics.map(d => {
        const newRow = {};
        newRow.tableRowActionButtons =
          whereClause.groupBys.length > 0 ? (
            <Inspect
              data={d}
              groupBys={whereClause.groupBys}
              RenderLink={RenderLink}
            />
          ) : null;
        // newRow.tableRowActionButtons = <Inspect requestId={d.request_id} />;
        Object.keys(d).forEach((elem, key) => {
          const renderElement = () => {
            if (elem === 'success') {
              if (!d[elem]) {
                return <img src={success} alt={'Success'} />;
              }
              return <img src={failure} alt={'Failure'} />;
            }
            if (elem in transformedVals) {
              return transformedVals[elem](d[elem]);
            }
            /*
            if (elem === 'time') {
              return moment(new Date(d[elem])).fromNow();
            }
            */
            return d[elem];
          };
          const getTitle = () => {
            if (typeof d[elem] === 'boolean') {
              return '';
            }
            return d[elem];
          };
          newRow[elem] = (
            <div key={key} className={styles.columnRow} title={getTitle()}>
              {renderElement()}
            </div>
          );
        });
        return newRow;
      });
    }
    return [];
  };

  const getCount = () => {
    if ('count' in data.searchUsageMetricsAggregate) {
      return data.searchUsageMetricsAggregate.count;
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
    if (data.searchUsageMetrics.length === 0) {
      console.error('Minimum one row required to sort');
      return;
    }
    const rowEntry = data.searchUsageMetrics[0];
    const columnNames = Object.keys(rowEntry).map(column => column);

    if (!columnNames.includes(currColumn)) {
      return;
    }

    const col =
      currColumn in aliasedNames ? aliasedNames[currColumn] : currColumn;
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

  const renderOperationTable = () => {
    return (
      <div className={styles.clearBoth + ' ' + styles.addPaddTop}>
        <div className={styles.displayFlex + ' ' + styles.addPaddBottom}>
          <div className={styles.subHeader}>{label}</div>
          <div className="ml-4">
            <Button onClick={() => refetch()}>{refetchRender()}</Button>
          </div>
        </div>
        <div className={tableScss.tableContainer + ' ' + styles.tableFullWidth}>
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
      <div className="col-xs-12">{renderOperationTable()}</div>
    </div>
  );
};

export default BrowserRows;
