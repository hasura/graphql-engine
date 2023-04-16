import React, { useState, useEffect } from 'react';

import { useQuery } from '@apollo/react-hooks';

import { Button } from '@hasura/console-legacy-ce';

import { fetchOperations, getProjectConfigs } from './graphql.queries';
import Inspect from './Inspect';

import {
  stripUnderScore,
  capitalize,
  transformedVals,
  transformedHeaderVal,
  getWhereClause,
} from './utils';

import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import {
  // NO_ROLE_SYMBOL,
  aliasedNames,
  headerTitleLabel,
  OPERATIONS_FILTER_MAP,
  excludeRenderingColumns,
} from './constants';
import { FaCaretDown, FaCaretUp, FaSort } from 'react-icons/fa';

import styles from '../Metrics.module.scss';
import failure from '../images/failure.svg';
import success from '../images/success.svg';

const LIMIT = 10;

const defaultState = {
  limit: LIMIT,
  offset: 0,
  order_by: {
    time: 'desc',
  },
};

const BrowserRows = props => {
  const [browseState, setState] = useState(defaultState);
  const [page, setPage] = useState(1);
  const { filters, label, projectId, projectConfig } = props;

  /*  Get log sending settings data */

  useQuery(getProjectConfigs, {
    variables: {
      projectId: projectId,
    },
  });

  /* Get the list of filter types by filtering the type key of the filters list itself */

  const filtersObj = getWhereClause(OPERATIONS_FILTER_MAP, filters);

  const whereClause = {
    _and: [...filtersObj],
  };

  if (projectId) {
    /* Filter by project id */
    whereClause.project_id = {
      _eq: projectId,
    };
  }
  /*
  const timeRangeFilter = filterByType(filters, FILTER_TIME_RANGE_SYMBOL);
  timeRangeFilter.forEach(e => {
    whereClause.time = {
      _gt: getTimeRangeValue(e.value),
    };
  });
  */

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

  useEffect(() => {
    setPage(1);
    updateOffset(0);
  }, [filters]);

  const variables = {
    limit: limit,
    offset: offset,
    where: whereClause,
    orderBy: order_by,
  };
  const { loading, error, data, refetch, networkStatus } = useQuery(
    fetchOperations,
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
    if (data.operation_logs.length > 0) {
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

        const headerWithUnit = () => {
          if (header in transformedHeaderVal) {
            return transformedHeaderVal[header](header);
          }
          return header;
        };

        const headerCellWidth =
          getTextWidth(headerWithUnit(), HEADER_FONT) + HEADER_PADDING;

        return Math.min(
          MAX_WIDTH,
          Math.max(maxContentCellWidth, headerCellWidth)
        );
      };
      const columns = data.operation_logs[0];
      const headerRows = Object.keys(columns)
        .filter(c => excludeRenderingColumns.indexOf(c) === -1)
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
            width: getColWidth(c, data.operation_logs),
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

  const renderActionButtons = r => {
    /*
    const renderActionButtonForGroups = () => {
      if (groups.length > 0 && r.operation) {
        if (r.operation.operation_groups_operations.length > 0) {
          return (
            <DeleteFromOperationGroup
              projectId={groups[0].project_id}
              operationGroupName={groups[0].name}
              operationName={r.operation_name}
              refetch={refetch}
            />
          );
        }
        return (
          <AddToOperationGroup
            projectId={groups[0].project_id}
            operationGroupName={groups[0].name}
            operationName={r.operation_name}
            refetch={refetch}
          />
        );
      }
      return null;
    };
    */
    return (
      <div className={styles.textCenter}>
        <Inspect
          projectId={projectId}
          requestId={r.request_id}
          transport={r.transport}
          time={r.time}
          projectConfigData={projectConfig}
        />
        {/* renderActionButtonForGroups() */}
      </div>
    );
  };

  const getRows = () => {
    if (data.operation_logs.length > 0) {
      return data.operation_logs.map(d => {
        const newRow = {};
        newRow.tableRowActionButtons = renderActionButtons(d);
        Object.keys(d)
          .filter(cl => excludeRenderingColumns.indexOf(cl) === -1)
          .forEach((elem, key) => {
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
              <div
                key={key}
                className={`${styles.columnRow} ${
                  elem === 'success' && styles.textCenter
                }`}
                title={getTitle()}
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

  const _rows = getRows();
  const _columns = getHeaders();

  const handlePageNext = () => {
    if (page * limit !== offset) {
      updateOffset(page * limit);
    }
    setPage(page + 1);
  };

  const handlePagePrevious = () => {
    updateOffset((page - 2) * limit);
    setPage(page - 1);
  };

  // const onPageJump = () => {
  //   if (page > 1) {
  //     updateOffset((page - 1) * limit);
  //   } else if (page === 1) {
  //     updateOffset(0);
  //   }
  // };

  const handlePageSizeChange = size => {
    if (limit !== size) {
      updateLimit(size);
    }
  };

  let disableSortColumn = false;

  const sortByColumn = currColumn => {
    if (data.operation_logs.length === 0) {
      console.error('Minimum one row required to sort');
      return;
    }
    const rowEntry = data.operation_logs[0];
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

  const renderPreviousButton = () => {
    if (page > 1 && data.operation_logs.length === 0) {
      return (
        <Button disabled={page === 1} onClick={handlePagePrevious}>
          Previous
        </Button>
      );
    }
    return null;
  };

  const renderOperationTable = () => {
    return (
      <div className={styles.clearBoth}>
        <div className={styles.displayFlex + ' ' + styles.addPaddBottom}>
          <div className={styles.subHeader}>{label}</div>
          <div className="ml-4">
            <Button onClick={() => refetch()}>{refetchRender()}</Button>
          </div>
        </div>
        <div className={tableScss.tableContainer + ' ' + styles.tableFullWidth}>
          {_rows.length > 0 ? (
            <div>
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
                pages={null}
                onPageSizeChange={handlePageSizeChange}
                page={Math.floor(offset / limit)}
                showPagination={false}
              />
              <div className={styles.paginationFooter}>
                <div className={styles.textCenterAlign}>
                  <Button disabled={page === 1} onClick={handlePagePrevious}>
                    Previous
                  </Button>
                </div>
                <div className={styles.pageInfo}>
                  Page
                  <div className={styles.pageNumber}>{page}</div>
                </div>
                <div>
                  <Button
                    disabled={data.operation_logs.length < 10}
                    onClick={handlePageNext}
                  >
                    Next
                  </Button>
                </div>
              </div>
            </div>
          ) : (
            <div>
              <div>No results found!</div>
              {renderPreviousButton()}
            </div>
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
