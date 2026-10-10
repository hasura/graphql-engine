import React, { useRef, useState } from 'react';
import { useQuery } from '@apollo/client/react';
import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';
import { fetchQueryList } from './graphql.queries';
import Inspect from './Inspect';
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

import {
  FILTER_MAP,
  aliasedNames,
  headerTitleLabel,
  defaultColumns,
} from './constants';

import { TIME_RANGE_SYMBOL } from '../constants';

import { FaCaretDown, FaCaretUp, FaSort } from 'react-icons/fa';

import styles from '../Metrics.module.scss';
import failure from '../images/failure.svg';
import success from '../images/success.svg';
import { Button } from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';

const LIMIT = 10;

const BrowserRows = (props) => {
  const defaultState: Record<string, any> = {
    limit: LIMIT,
    offset: 0,
    order_by: {
      operation_name: 'asc',
    },
    now: new Date().toISOString(),
  };

  const [browseState, setState] = useState(defaultState);
  // if given, use cached canvas for better performance
  // else, create new canvas
  const canvas = useRef<HTMLCanvasElement>(document.createElement('canvas'));
  const { filters, groupBys, label, projectId, RenderLink } = props;

  /* Get the list of filter types by filtering the type key of the filters list itself */

  const getFilterObj = getWhereClauseEx(FILTER_MAP, filters);

  const getGroupBys = () => {
    const groupBy = groupBys.map((i) => {
      return i;
    });
    return groupBy;
  };

  const whereClause = {
    fromTime: getTimeRangeValue(),
    toTime: browseState['now'],
    groupBys: [...getGroupBys()],
  };
  const timeRangeFilter = filterByType(filters, TIME_RANGE_SYMBOL);

  timeRangeFilter.forEach((e) => {
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

  const variables = {
    limit: limit,
    offset: offset,
    ...whereClause,
    groupBys: whereClause.groupBys,
    ...getFilterObj,
    orderBy: order_by,
    project_id: [] as string[],
  };
  if (projectId) {
    variables.project_id = [projectId];
  }
  const { loading, error, data, refetch, networkStatus } = useQuery(
    fetchQueryList,
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

  const getHeaders = () => {
    if ((data as any)?.searchUsageMetrics?.length > 0) {
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
            const content: any = contentRows[i][header];

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

        const headerWithUnit = (h) => {
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
          Math.max(maxContentCellWidth, headerCellWidth),
        );
      };
      let validColumns = [...defaultColumns];
      if (whereClause.groupBys.length > 0) {
        validColumns = [...whereClause.groupBys, ...defaultColumns];
      }
      const searchUsageMetrics = (data as any)?.searchUsageMetrics ?? [];
      const columns = searchUsageMetrics[0];
      const headerRows = Object.keys(columns)
        .filter((f) => validColumns.indexOf(getIfAliased(f)) !== -1)
        .map((c, key) => {
          let sortIcon: React.ReactNode = <FaSort />;
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
            header: () => (
              <div
                key={key}
                className={`${styles['columnHeader']} ellipsis`}
                title="Click to sort"
              >
                {getWithUnitHeaderTitle()}
                <span className={tableScss['tableHeaderCell']}>{sortIcon}</span>
              </div>
            ),
            accessorKey: c,
            cell: (info) => info.getValue(),
            id: c,
            foldable: true,
            size: getColWidth(c, searchUsageMetrics),
          };
        });
      const actionRow = {
        header: () => (
          <div
            key={'action_operation_header'}
            className={`${styles['columnHeader']}`}
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
    }
    return [];
  };

  const getRows = () => {
    const searchUsageMetrics = (data as any)?.searchUsageMetrics ?? [];
    if (searchUsageMetrics.length > 0) {
      return searchUsageMetrics.map((d) => {
        const newRow = {};
        newRow['tableRowActionButtons'] =
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
            <div key={key} className={styles['columnRow']} title={getTitle()}>
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
    const searchUsageMetricsAggregate =
      (data as any)?.searchUsageMetricsAggregate ?? {};
    if ('count' in searchUsageMetricsAggregate) {
      return searchUsageMetricsAggregate.count;
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

  const sortByColumn = (currColumn: string) => {
    const searchUsageMetrics = (data as any)?.searchUsageMetrics ?? [];
    if (searchUsageMetrics.length === 0) {
      console.error('Minimum one row required to sort');
      return;
    }
    const rowEntry = searchUsageMetrics[0];
    const columnNames = Object.keys(rowEntry).map((column) => column);

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
          onClick={() => handlePageChange(currentPage + 1)}
          disabled={currentPage + 1 >= totalPages}
          data-test="view-rows-pagination-next"
        >
          Next
        </Button>
      </div>
    );
  };

  const renderOperationTable = () => {
    return (
      <div className={styles['clearBoth'] + ' ' + styles['addPaddTop']}>
        <Flex
          align="center"
          justify="center"
          className={styles['addPaddBottom']}
        >
          <div className={styles['subHeader']}>{label}</div>
          <div className="ml-4">
            <Button onClick={() => refetch()}>{refetchRender()}</Button>
          </div>
        </Flex>
        <div
          className={
            tableScss['tableContainer'] + ' ' + styles['tableFullWidth']
          }
        >
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
      <div className="col-xs-12">{renderOperationTable()}</div>
    </div>
  );
};

export default BrowserRows;
