import React from 'react';
import ReactTable from 'react-table';
import AceEditor from 'react-ace';
import Tabs from 'react-bootstrap/lib/Tabs';
import Tab from 'react-bootstrap/lib/Tab';
import 'brace/mode/json';
import 'react-table/react-table.css';
import FilterQuery from './FilterQuery';
import {
  setOrderCol,
  setOrderType,
  removeOrder,
  runQuery,
  setOffset,
  setLimit,
  addOrder,
} from './FilterActions';
import {
  ordinalColSort,
  convertDateTimeToLocale,
  verifySuccessStatus,
  parseRowData,
} from '../utils';
import '../TableCommon/EventReactTableOverrides.css';
import * as tooltip from '../Common/Tooltips';
import OverlayTrigger from 'react-bootstrap/lib/OverlayTrigger';

const ViewRows = ({
  curTriggerName,
  curQuery,
  curFilter,
  curRows,
  curPath,
  curDepth,
  activePath,
  currentTrigger,
  dispatch,
  isView,
  count,
  expandedRow,
}) => {
  const styles = require('../TableCommon/EventTable.scss');
  const curRelName = curPath.length > 0 ? curPath.slice(-1)[0] : null;

  // Am I a single row display
  const isSingleRow = false;

  // Get the headings
  const tableHeadings = [];
  const gridHeadings = [];
  const eventLogColumns = ['event_id', 'delivered', 'created_at'];
  const sortedColumns = eventLogColumns.sort(ordinalColSort);

  sortedColumns.map((column, i) => {
    tableHeadings.push(<th key={i}>{column}</th>);
    gridHeadings.push({
      Header: column,
      accessor: column,
    });
  });

  const hasPrimaryKeys = true;
  /*
  let editButton;
  let deleteButton;
  */

  const newCurRows = [];
  if (curRows && curRows[0] && curRows[0].events) {
    curRows[0].events.forEach((row, rowIndex) => {
      const newRow = {};
      const pkClause = {};
      if (!isView && hasPrimaryKeys) {
        pkClause.id = row.id;
      } else {
        currentTrigger.map(k => {
          pkClause[k] = row[k];
        });
      }
      /*
      if (!isSingleRow && !isView && hasPrimaryKeys) {
        deleteButton = (
          <button
            className={`${styles.add_mar_right_small} btn btn-xs btn-default`}
            onClick={() => {
              dispatch(deleteItem(pkClause));
            }}
            data-test={`row-delete-button-${rowIndex}`}
          >
            Delete
          </button>
        );
      }
      const buttonsDiv = (
        <div className={styles.tableCellCenterAligned}>
          {editButton}
          {deleteButton}
        </div>
      );
      */
      // Insert Edit, Delete, Clone in a cell
      // newRow.actions = buttonsDiv;
      // Insert cells corresponding to all rows
      sortedColumns.forEach(col => {
        const getCellContent = () => {
          let conditionalClassname = styles.tableCellCenterAlignedOverflow;
          const cellIndex = `${curTriggerName}-${col}-${rowIndex}`;
          if (expandedRow === cellIndex) {
            conditionalClassname = styles.tableCellExpanded;
          }
          if (row[col] === null) {
            return (
              <div className={conditionalClassname}>
                <i>NULL</i>
              </div>
            );
          }
          let content = row[col] === undefined ? 'NULL' : row[col].toString();
          if (col === 'created_at') {
            content = convertDateTimeToLocale(row[col]);
          }
          if (col === 'event_id') {
            content = row.id.toString();
          }
          if (col === 'delivered') {
            content = row[col] ? (
              <i
                className={
                  styles.invocationSuccess + ' fa fa-check invocationsSuccess'
                }
              />
            ) : (
              <i
                className={
                  styles.invocationFailure + ' fa fa-times invocationsFailure'
                }
              />
            );
          }
          return <div className={conditionalClassname}>{content}</div>;
        };
        newRow[col] = getCellContent();
      });
      newCurRows.push(newRow);
    });
  }

  // Is this ViewRows visible
  let isVisible = false;
  if (!curRelName) {
    isVisible = true;
  } else if (curRelName === activePath[curDepth]) {
    isVisible = true;
  }

  let filterQuery = null;
  if (!isSingleRow) {
    if (curRelName === activePath[curDepth] || curDepth === 0) {
      // Rendering only if this is the activePath or this is the root

      let wheres = [{ '': { '': '' } }];
      if ('where' in curFilter && '$and' in curFilter.where) {
        wheres = [...curFilter.where.$and];
      }

      let orderBy = [{ column: '', type: 'asc', nulls: 'last' }];
      if ('order_by' in curFilter) {
        orderBy = [...curFilter.order_by];
      }
      const limit = 'limit' in curFilter ? curFilter.limit : 10;
      const offset = 'offset' in curFilter ? curFilter.offset : 0;

      filterQuery = (
        <FilterQuery
          curQuery={curQuery}
          whereAnd={wheres}
          triggerSchema={currentTrigger}
          orderBy={orderBy}
          limit={limit}
          dispatch={dispatch}
          count={count}
          triggerName={curTriggerName}
          offset={offset}
        />
      );
    }
  }

  const sortByColumn = col => {
    // Remove all the existing order_bys
    const numOfOrderBys = curFilter.order_by.length;
    for (let i = 0; i < numOfOrderBys - 1; i++) {
      dispatch(removeOrder(1));
    }
    // Go back to the first page
    dispatch(setOffset(0));
    // Set the filter and run query
    dispatch(setOrderCol(col, 0));
    if (
      curFilter.order_by.length !== 0 &&
      curFilter.order_by[0].column === col &&
      curFilter.order_by[0].type === 'asc'
    ) {
      dispatch(setOrderType('desc', 0));
    } else {
      dispatch(setOrderType('asc', 0));
    }
    dispatch(runQuery());
    // Add a new empty filter
    dispatch(addOrder());
  };

  const changePage = page => {
    if (curFilter.offset !== page * curFilter.limit) {
      dispatch(setOffset(page * curFilter.limit));
      dispatch(runQuery());
    }
  };

  const changePageSize = size => {
    if (curFilter.size !== size) {
      dispatch(setLimit(size));
      dispatch(runQuery());
    }
  };

  const successIcon = (
    <i className={styles.invocationSuccess + ' fa fa-check'} />
  );

  const failureIcon = (
    <i className={styles.invocationFailure + ' fa fa-times'} />
  );

  const renderTableBody = () => {
    if (newCurRows.length === 0) {
      return <div> No rows found. </div>;
    }
    let shouldSortColumn = true;
    const invocationColumns = ['status', 'id', 'created_at'];
    const invocationGridHeadings = [];
    invocationColumns.map(column => {
      invocationGridHeadings.push({
        Header: column,
        accessor: column,
      });
    });
    return (
      <ReactTable
        className="-highlight processedEventsTable"
        data={newCurRows}
        columns={gridHeadings}
        resizable
        manual
        sortable={false}
        minRows={0}
        getTheadThProps={(finalState, some, column) => ({
          onClick: () => {
            if (
              column.Header &&
              shouldSortColumn &&
              column.Header !== 'Actions'
            ) {
              sortByColumn(column.Header);
            }
            shouldSortColumn = true;
          },
        })}
        getResizerProps={(finalState, none, column, ctx) => ({
          onMouseDown: e => {
            shouldSortColumn = false;
            ctx.resizeColumnStart(e, column, false);
          },
        })}
        showPagination={count > curFilter.limit}
        defaultPageSize={Math.min(curFilter.limit, count)}
        pages={Math.ceil(count / curFilter.limit)}
        onPageChange={changePage}
        onPageSizeChange={changePageSize}
        page={Math.floor(curFilter.offset / curFilter.limit)}
        SubComponent={row => {
          const currentIndex = row.index;
          const currentRow = curRows[0].events[currentIndex];
          const invocationRowsData = [];
          const requestData = [];
          const responseData = [];
          currentRow.logs.map((r, rowIndex) => {
            const newRow = {};
            const status = verifySuccessStatus(r.status)
              ? successIcon
              : failureIcon;

            requestData.push(parseRowData(r, 'request'));
            responseData.push(parseRowData(r, 'response'));
            // Insert cells corresponding to all rows
            invocationColumns.forEach(col => {
              const getCellContent = () => {
                let conditionalClassname =
                  styles.tableCellCenterAlignedOverflow;
                const cellIndex = `${curTriggerName}-${col}-${rowIndex}`;
                if (expandedRow === cellIndex) {
                  conditionalClassname = styles.tableCellExpanded;
                }
                if (r[col] === null) {
                  return (
                    <div className={conditionalClassname}>
                      <i>NULL</i>
                    </div>
                  );
                }
                if (col === 'status') {
                  return status;
                }
                if (col === 'created_at') {
                  const formattedDate = convertDateTimeToLocale(r.created_at);
                  return (
                    <div className={conditionalClassname}>{formattedDate}</div>
                  );
                }
                const content =
                  r[col] === undefined ? 'NULL' : r[col].toString();
                return <div className={conditionalClassname}>{content}</div>;
              };
              newRow[col] = getCellContent();
            });
            invocationRowsData.push(newRow);
          });
          return (
            <div style={{ padding: '20px' }}>
              <em>Recent Invocations</em>
              <div
                className={styles.invocationsSection + ' invocationsSection'}
              >
                {invocationRowsData.length ? (
                  <ReactTable
                    data={invocationRowsData}
                    columns={invocationGridHeadings}
                    defaultPageSize={currentRow.logs.length}
                    minRows={0}
                    showPagination={false}
                    SubComponent={logRow => {
                      const finalIndex = logRow.index;
                      const finalRequest = requestData[finalIndex];
                      const finalResponse = responseData[finalIndex];
                      return (
                        <div style={{ padding: '20px' }}>
                          <Tabs
                            animation={false}
                            defaultActiveKey={1}
                            id="requestResponseTab"
                          >
                            <Tab eventKey={1} title="Request">
                              {finalRequest.headers ? (
                                <div className={styles.add_mar_top}>
                                  <div className={styles.subheading_text}>
                                    Headers
                                  </div>
                                  <AceEditor
                                    mode="json"
                                    theme="github"
                                    name="payload"
                                    value={JSON.stringify(
                                      finalRequest.headers,
                                      null,
                                      4
                                    )}
                                    minLines={4}
                                    maxLines={20}
                                    width="100%"
                                    showPrintMargin={false}
                                    showGutter={false}
                                  />
                                </div>
                              ) : null}
                              <div className={styles.add_mar_top}>
                                <div className={styles.subheading_text}>
                                  Payload
                                </div>
                                <AceEditor
                                  mode="json"
                                  theme="github"
                                  name="payload"
                                  value={JSON.stringify(
                                    finalRequest.data,
                                    null,
                                    4
                                  )}
                                  minLines={4}
                                  maxLines={100}
                                  width="100%"
                                  showPrintMargin={false}
                                  showGutter={false}
                                />
                              </div>
                            </Tab>
                            <Tab eventKey={2} title="Response">
                              {finalResponse.headers ? (
                                <div className={styles.add_mar_top}>
                                  <div className={styles.subheading_text}>
                                    Headers
                                  </div>
                                  <AceEditor
                                    mode="json"
                                    theme="github"
                                    name="response"
                                    value={JSON.stringify(
                                      finalResponse.headers,
                                      null,
                                      4
                                    )}
                                    minLines={4}
                                    maxLines={20}
                                    width="100%"
                                    showPrintMargin={false}
                                    showGutter={false}
                                  />
                                </div>
                              ) : null}
                              <div className={styles.add_mar_top}>
                                <div
                                  className={
                                    styles.subheading_text +
                                    ' col-md-6 ' +
                                    styles.padd_remove
                                  }
                                >
                                  {finalResponse.status_code
                                    ? 'Payload'
                                    : 'Error'}
                                </div>
                                <div
                                  className={
                                    styles.status_code_right +
                                    ' col-md-6 ' +
                                    styles.padd_remove
                                  }
                                >
                                  {finalResponse.status_code
                                    ? [
                                      'Status Code: ',
                                      verifySuccessStatus(
                                        finalResponse.status_code
                                      )
                                        ? successIcon
                                        : failureIcon,
                                      finalResponse.status_code,
                                      ' ',
                                      <OverlayTrigger
                                        placement="top"
                                        overlay={
                                          tooltip.statusCodeDescription
                                        }
                                      >
                                        <i
                                          className="fa fa-question-circle"
                                          aria-hidden="true"
                                        />
                                      </OverlayTrigger>,
                                    ]
                                    : null}
                                </div>
                                <AceEditor
                                  mode="json"
                                  theme="github"
                                  name="response"
                                  value={JSON.stringify(
                                    finalResponse.data,
                                    null,
                                    4
                                  )}
                                  minLines={4}
                                  maxLines={100}
                                  width="100%"
                                  showPrintMargin={false}
                                  showGutter={false}
                                />
                              </div>
                            </Tab>
                          </Tabs>
                        </div>
                      );
                    }}
                  />
                ) : (
                  <div className={styles.add_mar_top}>No data available</div>
                )}
              </div>
              <br />
              <br />
            </div>
          );
        }}
      />
    );
  };

  return (
    <div className={isVisible ? '' : 'hide '}>
      {filterQuery}
      <hr />
      <div className="row">
        <div className="col-xs-12">
          <div className={styles.tableContainer + ' eventsTableBody'}>
            {renderTableBody()}
          </div>
          <br />
          <br />
        </div>
      </div>
    </div>
  );
};

export default ViewRows;
