import React, { useState, useEffect } from 'react';

import { useQuery } from '@apollo/react-hooks';
import { Link } from 'react-router';
import { FaSort, FaCaretDown, FaCaretUp } from 'react-icons/fa';

import { fetchOperations, fetchRemoteOperations } from './graphql.queries';

import { allowListOperationGroupName } from './constants';

import { Tooltip } from '@hasura/console-legacy-ce';

import { createFilter } from '../Error/utils';

import { excludedColumns } from './constants';

import LoadingSpinner from '../Common/LoadingSpinner';

import FilterCheckboxComponent from '../StatsPanel/Filter/FilterCheckboxComponent';
import AddOperationsToAllowList from './AddOperationsToAllowList';

import {
  stripUnderScore,
  capitalize,
  transformedHeaderVal,
} from '../Operations/utils';

import { transformedVals } from './utils';

import { aliasedNames } from './constants';

import { OPERATION_NAME_SYMBOL, relativeModulePath } from '../constants';

import { DragFoldTable, tableScss } from '@hasura/console-legacy-ce';

import { checkObjectValidity } from './utils';

import { ActionsPanel } from '../Common/ActionsPanel';

import syncOperationsIcon from '../images/sync.svg';
import styles from '../Metrics.module.scss';
import failure from '../images/failure.svg';
import success from '../images/success.svg';
import inspectRow from '../images/usage.svg';

const LIMIT = 5;

const defaultState = {
  limit: LIMIT,
  offset: 0,
  order_by: {
    created_at: 'desc',
  },
};

const BrowserRows = props => {
  const [browseState, setState] = useState(defaultState);
  const {
    dispatch,
    label,
    projectId,
    remoteProjectId,
    RenderLink,
    allowlist,
    updateAllowList,
    toggleAddClearAllowList,
    changeToAllowedOperations,
    updateMetadata,
    projectName,
  } = props;

  /* Get the list of filter types by filtering the type key of the filters list itself */

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
    orderBy: order_by,
    groupName: allowListOperationGroupName,
  };

  if (projectId) {
    variables.projectId = projectId;
  }

  const isRemoteProjectSelected = remoteProjectId !== projectId;

  if (isRemoteProjectSelected) {
    variables.projectId = projectId;
    variables.remoteProjectId = remoteProjectId;
  }

  const onCompleted = data => {
    const { results_aggregate } = data;
    if (results_aggregate && typeof results_aggregate === 'object') {
      try {
        updateMetadata(results_aggregate.aggregate.count);
      } catch (e) {
        updateMetadata('N/A');
      }
    }
  };
  let fetchQuery = null;

  if (isRemoteProjectSelected) {
    fetchQuery = fetchRemoteOperations;
  } else {
    fetchQuery = fetchOperations;
  }

  const { loading, error, data, refetch, networkStatus } = useQuery(
    fetchQuery,
    {
      variables: variables,
      notifyOnNetworkStatusChange: true,
      onCompleted: onCompleted,
      fetchPolicy: 'network-only',
    }
  );

  const reHydrateState = () => {
    refetch();
    return () => updateMetadata(null);
  };

  useEffect(reHydrateState, []);

  if (loading && !checkObjectValidity(data)) {
    return <span>Getting operations from your Hasura account...</span>;
  }
  if (error && !checkObjectValidity(data)) {
    return (
      <span className={styles.errorMessage}>
        Error fetching
        <code>{error.toString()}</code>
      </span>
    );
  }

  if (checkObjectValidity(data) && Object.keys(data).length === 0) {
    // If the data is empty for some reason don't handle and wait
    return null;
  }

  const getSaveFunc = () => {
    if (allowlist.length > 0) {
      const onError = err => console.error(err);
      const onSuccess = () => {
        toggleAddClearAllowList([]);
        changeToAllowedOperations();
        /*
        if (BrowseAllowListsSym in refetchState) {
          const refetchFn = refetchState[BrowseAllowListsSym];
          if (typeof refetchFn === 'function') {
            // Refetches the allow list data
            refetchFn();
          }
        }
        */
      };

      const p = {
        dispatch: dispatch,
        operations: allowlist,
        projectId,
        collectionName: allowListOperationGroupName,
        onErrorCb: onError,
        onCompletedCb: onSuccess,
        btnStates: {
          btnLoading: 'Adding...',
          btnSuccessful: 'Added Successfully',
          btnInit: 'Add to Allowlist',
        },
      };

      return (
        <div className={styles.addOperationsBtn}>
          <AddOperationsToAllowList {...props} {...p} />
        </div>
      );
    }
    return null;
  };

  const refetchRender = () => {
    if (networkStatus === 4) {
      return <LoadingSpinner />;
    }
    return (
      <img
        className={styles.actionImg}
        src={syncOperationsIcon}
        alt={'Reload operations'}
        onClick={() => refetch()}
      />
    );
  };

  const IconWrapper = ({ tooltip, children }) => {
    return (
      <Tooltip side="right" tooltipContentChildren={tooltip}>
        <span
          className={`${styles.refetchNewOperations} ${styles.deleteImgMr} ${styles.minHeightIconWrapper} ${styles.displayFlex}`}
        >
          {children}
        </span>
      </Tooltip>
    );
  };

  const renderReloadTooltip = () => {
    return (
      <React.Fragment>
        Reload new operations from <code>{projectName}</code>
      </React.Fragment>
    );
  };

  const renderActionBar = () => {
    const refetchNewOperations = () => (
      <IconWrapper tooltip={renderReloadTooltip()}>
        {refetchRender()}
      </IconWrapper>
    );
    return (
      <ActionsPanel>
        {refetchNewOperations()}
        {getSaveFunc()}
      </ActionsPanel>
    );
  };

  const getHeaders = () => {
    if (data.results.length > 0) {
      const getColWidth = (header, contentRows = []) => {
        const MAX_WIDTH = 500;
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
      const columns = data.results[0];
      const headerRows = Object.keys(columns)
        .filter(c1 => excludedColumns.indexOf(c1) === -1)
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
            width: getColWidth(c, data.results),
          };
        });
      const onChangeAllSelection = () => {
        const getOperations = () => {
          return data.results.map(o => {
            return {
              name: o.name,
              query: o.query,
              operation_id: o.operation_id,
            };
          });
        };
        const operations = (data.results.length > 0 && getOperations()) || [];
        if (allowlist.length !== data.results.length) {
          toggleAddClearAllowList(operations);
        } else {
          toggleAddClearAllowList([]);
        }
      };
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
        width: 100,
      };
      const selectRowAction = {
        Header: (
          <div
            key={'action_operation_header'}
            className={`${styles.columnHeader} ${styles.textCenter}`}
          >
            <FilterCheckboxComponent
              title={''}
              id={'all'}
              onChange={onChangeAllSelection}
              checked={allowlist.length === data.results.length}
              bsClass={styles.bsCheckBoxClass}
            />
          </div>
        ),
        accessor: 'tableRowSelectAction',
        id: 'tableRowSelectAction',
        width: 80,
      };
      return [selectRowAction, actionRow, ...headerRows];
    }
    return [];
  };

  const renderIcon = name => {
    const getOperationUrl = () => {
      const filters = [createFilter(OPERATION_NAME_SYMBOL, name)];
      return {
        pathname: `${relativeModulePath}/operations`,
        search: `?filters=${window.encodeURI(JSON.stringify(filters))}`,
      };
    };
    const renderLink = () => {
      const linkParam = getOperationUrl();
      const { search } = linkParam;
      const icon = (
        <img
          className={styles.actionImg}
          src={inspectRow}
          alt={'Show operations'}
        />
      );
      if (RenderLink) {
        return (
          <RenderLink pageName="operations" search={search}>
            {icon}
          </RenderLink>
        );
      }
      return <Link to={linkParam}>{icon}</Link>;
    };
    return (
      <div className={styles.iconPadding}>
        <Tooltip side="right" tooltipContentChildren="View operations">
          {renderLink()}
        </Tooltip>
      </div>
    );
  };

  const renderActionButtonForGroups = operation => {
    const parseCheckboxResponse = stringifiedObj => {
      try {
        const parsed = JSON.parse(stringifiedObj);
        updateAllowList(parsed);
      } catch (e) {
        console.error('Something went wrong! possible malformed data');
      }
    };
    return (
      <FilterCheckboxComponent
        title={''}
        id={JSON.stringify(operation)}
        onChange={parseCheckboxResponse}
        checked={allowlist.findIndex(o => o.name === operation.name) !== -1}
        bsClass={styles.bsCheckBoxClass}
      />
    );
  };

  const getRows = () => {
    if (data.results.length > 0) {
      return data.results.map(d => {
        const newRow = {};
        newRow.tableRowActionButtons = (
          <div className={styles.textCenter}>{renderIcon(d.name)}</div>
        );
        newRow.tableRowSelectAction = (
          <div className={styles.textCenter}>
            {renderActionButtonForGroups(d)}
          </div>
        );
        Object.keys(d)
          .filter(c1 => excludedColumns.indexOf(c1) === -1)
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
    if ('aggregate' in data.results_aggregate) {
      return data.results_aggregate.aggregate.count;
    }
    return 0;
  };

  const _rows = getRows();
  const _columns = getHeaders();

  const handlePageChange = page => {
    if (offset !== page * limit) {
      updateOffset(page * limit);
      toggleAddClearAllowList([]);
    }
  };

  const handlePageSizeChange = size => {
    if (limit !== size) {
      updateLimit(size);
      toggleAddClearAllowList([]);
    }
  };

  let disableSortColumn = false;

  const sortByColumn = currColumn => {
    if (data.results.length === 0) {
      console.error('Minimum one row required to sort');
      return;
    }
    const rowEntry = data.results[0];
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
    // If the result is a non empty list then render the table,
    // otherwise render the table with data
    if (_rows.length > 0) {
      return (
        <div className={tableScss.tableContainer + ' ' + styles.tableFullWidth}>
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
        </div>
      );
    }
    return (
      <div>
        <br />
        There are no operations performed on this server yet or all the
        operations are already part of the allow list! Try reloading the list
        using the button above.
      </div>
    );
  };

  return (
    <div className={`row ${tableScss.add_mar_top}`}>
      <div className="col-xs-12">
        <div className={styles.clearBoth}>
          {label && (
            <div className={styles.displayFlex + ' ' + styles.addPaddBottom}>
              <div className={styles.subHeader}>{label}</div>
            </div>
          )}
          {renderActionBar()}
          {renderOperationTable()}
        </div>
      </div>
    </div>
  );
};

export default BrowserRows;
