import React, { useState, useEffect } from 'react';
import 'react-table/react-table.css';
import '../../../Common/TableCommon/ReactTableOverrides.css';
import DragFoldTable, {
  getColWidth,
} from '../../../Common/TableCommon/DragFoldTable';

import Dropdown from '../../../Common/Dropdown/Dropdown';

import InvokeManualTrigger from '../../Events/EventTriggers/InvokeManualTrigger/InvokeManualTrigger';

import {
  vExpandRel,
  vCloseRel,
  V_SET_ACTIVE,
  deleteItems,
  deleteItem,
  vExpandRow,
  vCollapseRow,
} from './ViewActions'; // eslint-disable-line no-unused-vars

import {
  setOrderCol,
  setOrderType,
  removeOrder,
  runQuery,
  setOffset,
  setLimit,
  addOrder,
} from './FilterActions';

import _push from '../push';
import { ordinalColSort } from '../utils';
import FilterQuery from './FilterQuery';
import Spinner from '../../../Common/Spinner/Spinner';
import Button from '../../../Common/Button/Button';

import { E_SET_EDITITEM } from './EditActions';
import { I_SET_CLONE } from '../TableInsertItem/InsertActions';
import {
  getTableInsertRowRoute,
  getTableEditRowRoute,
} from '../../../Common/utils/routesUtils';
import {
  findTable,
  getRelationshipRefTable,
  dataSource,
} from '../../../../dataSources';
import { updateSchemaInfo } from '../DataActions';
import {
  persistColumnCollapseChange,
  getPersistedCollapsedColumns,
  persistColumnOrderChange,
  getPersistedColumnsOrder,
  persistPageSizeChange,
} from './tableUtils';
import { compareRows, isTableWithPK } from './utils';
import styles from '../../../Common/TableCommon/Table.scss';

const ViewRows = props => {
  const {
    curTableName,
    currentSchema,
    curQuery,
    curFilter,
    curRows,
    curPath = [],
    parentTableName,
    curDepth,
    activePath,
    schemas,
    dispatch,
    ongoingRequest,
    isProgressing,
    lastError,
    lastSuccess,
    isView,
    count,
    expandedRow,
    manualTriggers = [],
    location,
    readOnlyMode,
    shouldHidePagination,
    currentSource,
    useCustomPagination,
  } = props;
  const [invokedRow, setInvokedRow] = useState(null);
  const [invocationFunc, setInvocationFunc] = useState(null);
  const [selectedRows, setSelectedRows] = useState([]);
  useEffect(() => {
    setSelectedRows([]);
  }, [curTableName, currentSchema]);

  const NO_PRIMARY_KEY_MSG = 'No primary key to identify row';

  // Invoke manual trigger status
  const invokeTrigger = (trigger, row) => {
    setInvokedRow(row);
    setInvocationFunc(trigger);
  };

  const onCloseInvokeTrigger = () => {
    setInvokedRow(null);
    setInvokedRow(null);
  };

  const handleAllCheckboxChange = e => {
    if (e.target.checked) {
      setSelectedRows(curRows);
    } else {
      setSelectedRows([]);
    }
  };

  const checkIfSingleRow = _curRelName => {
    let _isSingleRow = false;

    const parentTableSchema = parentTableName
      ? schemas.find(t => t.table_name === parentTableName)
      : null;

    if (curQuery.columns.find(c => typeof c === 'object')) {
      // Do I have any children
      _isSingleRow = true;
    } else if (
      _curRelName &&
      parentTableSchema &&
      parentTableSchema.relationships.find(
        r => r.rel_name === _curRelName && r.rel_type === 'object'
      )
    ) {
      // Am I an obj_rel for my parent?
      _isSingleRow = true;
    }

    return _isSingleRow;
  };

  const getGridHeadings = (_columns, _relationships, _disableBulkSelect) => {
    const _gridHeadings = [];

    _gridHeadings.push({
      Header: '',
      accessor: 'tableRowActionButtons',
      id: 'tableRowActionButtons',
      width: 152,
    });

    _gridHeadings.push({
      Header: (
        <div className={styles.tableCenterContent}>
          <input
            className={`${styles.inputCheckbox} ${styles.headerInputCheckbox} legacy-input-fix`}
            checked={
              curRows.length > 0 && selectedRows.length === curRows.length
            }
            disabled={_disableBulkSelect}
            title={_disableBulkSelect ? 'No primary key to identify row' : ''}
            type="checkbox"
            onChange={handleAllCheckboxChange}
            data-test="select-all-rows"
          />
        </div>
      ),
      accessor: 'tableRowSelectAction',
      id: 'tableRowSelectAction',
      width: 60,
    });

    _columns.map(col => {
      const columnName = col.column_name;

      let sortIcon = 'fa-sort';
      if (curQuery.order_by && curQuery.order_by.length) {
        curQuery.order_by.forEach(orderBy => {
          if (orderBy.column === columnName) {
            sortIcon = orderBy.type === 'asc' ? 'fa-caret-up' : 'fa-caret-down';
          }
        });
      }

      _gridHeadings.push({
        Header: (
          <div className="ellipsis">
            <span className={styles.tableHeaderCell}>
              {columnName} <i className={'fa ' + sortIcon} />
            </span>
          </div>
        ),
        accessor: columnName,
        id: columnName,
        foldable: true,
        width: getColWidth(columnName, curRows),
      });
    });

    _relationships.map(rel => {
      const relName = rel.rel_name;

      _gridHeadings.push({
        Header: (
          <div className="ellipsis">
            <span className={styles.tableHeaderCell}>{relName}</span>
          </div>
        ),
        accessor: relName,
        id: relName,
        foldable: true,
        width: getColWidth(relName),
      });
    });

    return _gridHeadings;
  };

  const handleCheckboxChange = (row, e, tableSchema) => {
    if (e.target.checked) {
      setSelectedRows(prev => [...prev, row]);
    } else {
      setSelectedRows(prev =>
        prev.filter(prevRow => !compareRows(prevRow, row, tableSchema, isView))
      );
    }
  };

  const getPKClause = (row, hasPrimaryKey, tableSchema) => {
    const pkClause = {};

    if (!isView && hasPrimaryKey) {
      tableSchema.primary_key.columns.forEach(key => {
        pkClause[key] = row[key];
      });
    } else if (tableSchema.unique_constraints?.length) {
      tableSchema.unique_constraints[0].columns.forEach(key => {
        pkClause[key] = row[key];
      });
    } else {
      tableSchema.columns
        .filter(c => !dataSource.isJsonColumn(c))
        .forEach(key => {
          pkClause[key.column_name] = row[key.column_name];
        });
    }

    Object.keys(pkClause).forEach(key => {
      if (Array.isArray(pkClause[key])) {
        pkClause[key] = dataSource.arrayToPostgresArray(pkClause[key]);
      }
    });

    return pkClause;
  };

  const getGridRows = (
    _tableSchema,
    _hasPrimaryKey,
    _isSingleRow,
    _disableBulkSelect
  ) => {
    const _gridRows = [];

    curRows.forEach((row, rowIndex) => {
      const newRow = {};

      const rowCellIndex = `${curTableName}-${rowIndex}`;
      const isExpanded = expandedRow === rowCellIndex;

      const getActionButtons = () => {
        let editButton;
        let cloneButton;
        let deleteButton;
        let expandButton;

        const getActionButton = (
          type,
          icon,
          title,
          handleClick,
          requirePK = false
        ) => {
          const disabled = requirePK && !_hasPrimaryKey;

          const disabledOnClick = e => {
            e.preventDefault();
            e.stopPropagation();
          };

          return (
            <Button
              className={styles.add_mar_right_small}
              color="white"
              size="xs"
              onClick={disabled ? disabledOnClick : handleClick}
              title={disabled ? 'No primary key to identify row' : title}
              data-test={`row-${type}-button-${rowIndex}`}
              disabled={disabled}
            >
              {icon}
            </Button>
          );
        };

        const getExpandButton = () => {
          let icon;
          let title;
          let handleClick;

          const handleExpand = () => {
            dispatch(vExpandRow(rowCellIndex));
          };
          const handleCollapse = () => {
            dispatch(vCollapseRow());
          };

          if (isExpanded) {
            icon = 'fa-compress';
            title = 'Collapse row';
            handleClick = handleCollapse;
          } else {
            icon = 'fa-expand';
            title = 'Expand row';
            handleClick = handleExpand;
          }

          const expanderIcon = <i className={`fa ${icon}`} />;

          return getActionButton('expand', expanderIcon, title, handleClick);
        };

        const getEditButton = pkClause => {
          const editIcon = <i className="fa fa-edit" />;

          const handleEditClick = () => {
            dispatch({ type: E_SET_EDITITEM, oldItem: row, pkClause });
            dispatch(
              _push(
                getTableEditRowRoute(
                  currentSchema,
                  currentSource,
                  curTableName,
                  true
                )
              )
            );
          };

          const editTitle = 'Edit row';

          return getActionButton(
            'edit',
            editIcon,
            editTitle,
            handleEditClick,
            true
          );
        };

        const getDeleteButton = pkClause => {
          const deleteIcon = <i className="fa fa-trash" />;

          const handleDeleteClick = () => {
            setSelectedRows(prev =>
              prev.filter(r => !compareRows(r, pkClause, _tableSchema, isView))
            );
            dispatch(deleteItem(pkClause, curTableName, currentSchema));
          };

          const deleteTitle = 'Delete row';

          return getActionButton(
            'delete',
            deleteIcon,
            deleteTitle,
            handleDeleteClick,
            true
          );
        };

        const getCloneButton = () => {
          const cloneIcon = <i className="fa fa-clone" />;

          const handleCloneClick = () => {
            dispatch({ type: I_SET_CLONE, clone: row });
            dispatch(
              _push(
                getTableInsertRowRoute(
                  currentSchema,
                  currentSource,
                  curTableName,
                  true
                )
              )
            );
          };

          const cloneTitle = 'Clone row';

          return getActionButton(
            'clone',
            cloneIcon,
            cloneTitle,
            handleCloneClick,
            true
          );
        };

        const getManualTriggersButton = () => {
          if (!manualTriggers.length) {
            return;
          }

          const triggerOptions = manualTriggers.map(m => {
            return {
              content: (
                <div>
                  <Button
                    color="white"
                    size="xs"
                    data-test={`run_manual_trigger_${m.name}`}
                    onClick={() => invokeTrigger(m.name, rowIndex)}
                  >
                    Invoke
                  </Button>
                  {m.name}
                </div>
              ),
            };
          });

          const triggerIcon = <i className="fa fa-caret-square-o-right" />;
          const triggerTitle = 'Invoke event trigger';

          const triggerBtn = getActionButton(
            'trigger',
            triggerIcon,
            triggerTitle,
            () => {}
          );

          return (
            <div className={styles.display_inline}>
              <Dropdown
                testId={`data_browse_rows_trigger_${rowIndex}`}
                options={triggerOptions}
                position="right"
                keyPrefix={`invoke_data_dropdown_${rowIndex}`}
              >
                {triggerBtn}
              </Dropdown>
              {invokedRow === rowIndex && (
                <InvokeManualTrigger
                  source={currentSource}
                  args={row}
                  name={`${invocationFunc}`}
                  onClose={onCloseInvokeTrigger}
                  identifier={`invoke_function_${invocationFunc}`}
                />
              )}
            </div>
          );
        };

        const showActionBtns = !readOnlyMode && !_isSingleRow && !isView;

        if (showActionBtns) {
          const pkClause = getPKClause(row, _hasPrimaryKey, _tableSchema);

          editButton = getEditButton(pkClause);
          deleteButton = getDeleteButton(pkClause);
          cloneButton = getCloneButton();
        }

        // eslint-disable-next-line prefer-const
        expandButton = getExpandButton();

        return (
          <div
            key={rowIndex}
            className={`${styles.tableCenterContent} ${styles.overflowUnset}`}
          >
            {cloneButton}
            {editButton}
            {deleteButton}
            {expandButton}
            {getManualTriggersButton()}
          </div>
        );
      };

      // Insert Edit, Delete, Clone in a cell
      newRow.tableRowActionButtons = getActionButtons();

      // Check for bulk actions
      newRow.tableRowSelectAction = (
        <div className={styles.tableCenterContent}>
          <input
            className={`${styles.inputCheckbox} legacy-input-fix`}
            type="checkbox"
            disabled={_disableBulkSelect}
            title={_disableBulkSelect ? NO_PRIMARY_KEY_MSG : ''}
            checked={selectedRows.some(selectedRow =>
              compareRows(selectedRow, row, _tableSchema, isView)
            )}
            onChange={e => handleCheckboxChange(row, e, _tableSchema, isView)}
            data-test={`row-checkbox-${rowIndex}`}
          />
        </div>
      );

      // Insert column cells
      _tableSchema.columns.forEach(col => {
        const columnName = col.column_name;

        /* Row is a JSON object with `key` as the column name in the db
         * and `value` as corresponding column value of the column in the database,
         * Ex: author table with the following schema:
         *  id int Primary key,
         *  name text,
         *  address json
         *  `row`:
         *    {
         *      id: 1,
         *      name: "Hasura",
         *      address: {Hello: "World", Foo: "Bar"}
         *    }
         * */

        const getColCellContent = () => {
          const rowColumnValue = row[columnName];

          let cellValue = '';
          let cellTitle = '';

          if (rowColumnValue === null) {
            cellValue = <i>NULL</i>;
            cellTitle = 'NULL';
          } else if (rowColumnValue === undefined) {
            cellValue = 'NULL';
            cellTitle = cellValue;
          } else if (
            col.data_type === 'json' ||
            col.data_type === 'jsonb' ||
            typeof rowColumnValue === 'object'
          ) {
            cellValue = JSON.stringify(rowColumnValue, null, 4);
            cellTitle = cellValue;
          } else {
            cellValue = rowColumnValue.toString();
            cellTitle = cellValue;
          }

          return (
            <div
              className={isExpanded ? styles.tableCellExpanded : ''}
              title={cellTitle}
            >
              {cellValue}
            </div>
          );
        };

        newRow[columnName] = getColCellContent();
      });

      // Insert relationship cells
      _tableSchema.relationships.forEach(rel => {
        const relName = rel.rel_name;

        const getRelCellContent = () => {
          let cellValue = '';

          const getRelExpander = (value, className, clickHandler) => {
            return (
              <a href="#" className={className} onClick={clickHandler}>
                {value}
              </a>
            );
          };

          const isRelExpanded =
            curQuery.columns.find(c => c.name === rel.rel_name) !== undefined;

          if (isRelExpanded) {
            const handleCloseClick = e => {
              e.preventDefault();
              dispatch(vCloseRel(curPath, rel.rel_name));
            };

            cellValue = getRelExpander(
              'Close',
              styles.expanded,
              handleCloseClick
            );
          } else {
            const currentFkey = rel.rel_def.foreign_key_constraint_on;
            const currentFkeyValue = row[currentFkey];

            if (currentFkeyValue === null) {
              // cannot be expanded as value is null
              cellValue = <i>NULL</i>;
            } else {
              // can be expanded
              const pkClause = getPKClause(row, _hasPrimaryKey, _tableSchema);

              const handleViewClick = e => {
                e.preventDefault();

                const childTableDef = getRelationshipRefTable(
                  _tableSchema,
                  rel
                );
                if (childTableDef.schema !== currentSchema) {
                  dispatch(
                    updateSchemaInfo({ schemas: [childTableDef.schema] })
                  ).then(() => {
                    dispatch(vExpandRel(curPath, rel.rel_name, pkClause));
                  });
                } else {
                  dispatch(vExpandRel(curPath, rel.rel_name, pkClause));
                }
              };

              cellValue = getRelExpander('View', '', handleViewClick);
            }
          }

          return <div>{cellValue}</div>;
        };

        newRow[relName] = getRelCellContent();
      });

      _gridRows.push(newRow);
    });

    return _gridRows;
  };

  const curRelName = curPath.length > 0 ? curPath.slice(-1)[0] : null;
  const tableSchema = schemas.find(
    x => x.table_name === curTableName && x.table_schema === currentSchema
  );

  const tableColumnsSorted = tableSchema.columns.sort(ordinalColSort);
  const tableRelationships = tableSchema.relationships;

  const hasPrimaryKey = isTableWithPK(tableSchema);

  const isSingleRow = checkIfSingleRow(curRelName);

  const disableBulkSelect = !hasPrimaryKey;

  const _gridHeadings = getGridHeadings(
    tableColumnsSorted,
    tableRelationships,
    disableBulkSelect
  );

  const _gridRows = getGridRows(
    tableSchema,
    hasPrimaryKey,
    isSingleRow,
    disableBulkSelect
  );

  const getFilterQuery = () => {
    let _filterQuery = null;

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

        const offset = 'offset' in curFilter ? curFilter.offset : 0;

        _filterQuery = (
          <FilterQuery
            curQuery={curQuery}
            whereAnd={wheres}
            tableSchema={tableSchema}
            orderBy={orderBy}
            dispatch={dispatch}
            count={count}
            tableName={curTableName}
            offset={offset}
            urlQuery={location && location.query}
          />
        );
      }
    }

    return _filterQuery;
  };

  const getSelectedRowsSection = () => {
    const handleDeleteItems = () => {
      const pkClauses = selectedRows.map(row =>
        getPKClause(row, hasPrimaryKey, tableSchema)
      );
      dispatch(deleteItems(pkClauses, curTableName, currentSchema));
      setSelectedRows([]);
    };

    let selectedRowsSection = null;

    if (selectedRows.length > 0) {
      selectedRowsSection = (
        <div className={`${styles.display_flex} ${styles.add_padd_left_18}`}>
          <b className={styles.padd_small_right}>Selected:</b>
          {selectedRows.length}
          <button
            className={`${styles.add_mar_right_small} btn btn-xs btn-default ${styles.bulkDeleteButton}`}
            title="Delete selected rows"
            onClick={handleDeleteItems}
            data-test="bulk-delete"
          >
            <i className="fa fa-trash" />
          </button>
        </div>
      );
    }

    return selectedRowsSection;
  };

  // If query object has expanded columns
  const getChildComponent = () => {
    let _childComponent = null;

    const childQueries = [];
    curQuery.columns.map(c => {
      if (typeof c === 'object') {
        childQueries.push(c);
      }
    });

    const childTabs = childQueries.map((q, i) => {
      const isActive = q.name === activePath[curDepth + 1] ? 'active' : null;
      return (
        <li key={i} className={isActive} role="presentation">
          <a
            href="#"
            onClick={e => {
              e.preventDefault();
              dispatch({ type: V_SET_ACTIVE, path: curPath, relname: q.name });
            }}
          >
            {[...activePath.slice(0, 1), ...curPath, q.name].join('.')}
          </a>
        </li>
      );
    });

    const childViewRows = childQueries.map((cq, i) => {
      // Render child only if data is available
      if (curRows[0] && curRows[0][cq.name]) {
        const rel = tableSchema.relationships.find(r => r.rel_name === cq.name);

        if (rel) {
          const isObjectRel = rel.rel_type === 'object';

          let childRows = curRows[0][cq.name];
          if (isObjectRel) {
            childRows = [childRows];
          }
          const childTableDef = getRelationshipRefTable(tableSchema, rel);

          const childTable = findTable(schemas, childTableDef);

          return (
            <ViewRows
              key={i}
              curTableName={childTable.table_name}
              currentSchema={childTable.table_schema}
              curQuery={cq}
              curFilter={curFilter}
              curPath={[...curPath, rel.rel_name]}
              curRows={childRows}
              parentTableName={curTableName}
              activePath={activePath}
              ongoingRequest={ongoingRequest}
              lastError={lastError}
              lastSuccess={lastSuccess}
              schemas={schemas}
              curDepth={curDepth + 1}
              dispatch={dispatch}
              expandedRow={expandedRow}
              readOnlyMode={readOnlyMode}
              currentSource={currentSource}
            />
          );
        }
      }

      return null;
    });

    if (childQueries.length > 0) {
      _childComponent = (
        <div>
          <ul className="nav nav-tabs">{childTabs}</ul>
          {childViewRows}
        </div>
      );
    }

    return _childComponent;
  };

  const renderTableBody = () => {
    if (isProgressing) {
      return (
        <div>
          {' '}
          <Spinner />{' '}
        </div>
      );
    }

    const collapsedColumns = getPersistedCollapsedColumns(
      curTableName,
      currentSchema
    );
    const columnsOrder = getPersistedColumnsOrder(curTableName, currentSchema);

    let disableSortColumn = false;

    const sortByColumn = (col, clearExisting = true) => {
      const columnNames = tableColumnsSorted.map(column => column.column_name);
      if (!columnNames.includes(col)) {
        return;
      }

      const numOfOrderBys = curFilter.order_by.length;

      let orderByCol = col;
      let orderByPos = numOfOrderBys - 1;
      let orderType = 'asc';

      let isExistingColumn = false;
      for (let i = 0; i < numOfOrderBys; i++) {
        const orderBy = curFilter.order_by[i];

        if (orderBy.column === col) {
          isExistingColumn = true;

          if (orderBy.type === 'asc') {
            orderByPos = i;
            orderType = 'desc';
          } else {
            orderByPos = i;
            orderByCol = null;
          }
          break;
        }
      }

      // Go back to the first page
      dispatch(setOffset(0));

      if (orderByCol) {
        // Set the order_by
        dispatch(setOrderCol(col, orderByPos));
        dispatch(setOrderType(orderType, orderByPos));
      }

      // remove order_bys
      if (clearExisting) {
        let clearIndex = 0;
        for (let i = 0; i < numOfOrderBys; i++) {
          if (i !== orderByPos || !orderByCol) {
            dispatch(removeOrder(clearIndex));
          } else {
            clearIndex = 1;
          }
        }
      } else {
        if (isExistingColumn) {
          dispatch(removeOrder(numOfOrderBys - 1));
        }

        if (!orderByCol) {
          dispatch(removeOrder(orderByPos));
        }
      }

      // Run query
      dispatch(runQuery(tableSchema));

      // Add a new empty order_by
      dispatch(addOrder());
    };

    const getTheadThProps = (finalState, some, column) => ({
      onClick: e => {
        if (!disableSortColumn && column.id) {
          sortByColumn(column.id, !e.shiftKey);
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

    const handlePageChange = page => {
      if (curFilter.offset !== page * curFilter.limit) {
        dispatch(setOffset(page * curFilter.limit));
        dispatch(runQuery(tableSchema));
        setSelectedRows([]);
      }
    };

    const handlePageSizeChange = size => {
      if (curFilter.size !== size) {
        dispatch(setLimit(size));
        dispatch(setOffset(0));
        dispatch(runQuery(tableSchema));
        setSelectedRows([]);
        persistPageSizeChange(size);
      }
    };

    const PaginationWithOnlyNav = () => {
      const newPage = curFilter.offset / curFilter.limit;
      return (
        <div className={`row`} style={{ maxWidth: '500px' }}>
          <div className="col-xs-2">
            <button
              className="btn"
              onClick={() => handlePageChange(newPage - 1)}
              disabled={curFilter.offset === 0}
            >
              prev
            </button>
          </div>
          <div className="col-xs-4">
            <select
              value={curFilter.limit}
              onChange={e => {
                e.persist();
                handlePageSizeChange(parseInt(e.target.value, 10) || 10);
              }}
              className="form-control"
            >
              <option disabled value="">
                --
              </option>
              <option value={5}>5 rows</option>
              <option value={10}>10 rows</option>
              <option value={20}>20 rows</option>
              <option value={25}>25 rows</option>
              <option value={50}>50 rows</option>
              <option value={100}>100 rows</option>
            </select>
          </div>
          <div className="col-xs-2">
            <button
              className="btn"
              onClick={() => handlePageChange(newPage + 1)}
              disabled={curRows.length === 0}
            >
              next
            </button>
          </div>
        </div>
      );
    };

    const paginationProps = {};
    if (useCustomPagination) {
      paginationProps.PaginationComponent = PaginationWithOnlyNav;
    }

    return (
      <DragFoldTable
        className="dataTable -highlight -fit-content"
        data={_gridRows}
        columns={_gridHeadings}
        headerTitle={'Click to sort / Drag to rearrange'}
        resizable
        manual
        sortable={false}
        minRows={0}
        getTheadThProps={getTheadThProps}
        getResizerProps={getResizerProps}
        showPagination={!isSingleRow}
        pageSize={curFilter.limit}
        pages={Math.ceil(count / curFilter.limit)}
        onPageChange={handlePageChange}
        onPageSizeChange={handlePageSizeChange}
        page={Math.floor(curFilter.offset / curFilter.limit)}
        onCollapseChange={collapsedData =>
          persistColumnCollapseChange(
            curTableName,
            currentSchema,
            collapsedData
          )
        }
        defaultCollapsed={collapsedColumns}
        onOrderChange={reorderData =>
          persistColumnOrderChange(curTableName, currentSchema, reorderData)
        }
        defaultReorders={columnsOrder}
        showPagination={!shouldHidePagination}
        {...paginationProps}
      />
    );
  };

  // Is this ViewRows visible
  let isVisible = false;
  if (!curRelName || curRelName === activePath[curDepth]) {
    isVisible = true;
  }

  return (
    <div className={isVisible ? '' : 'hide '}>
      {getFilterQuery()}
      <div className={`row ${styles.add_mar_top}`}>
        {getSelectedRowsSection()}
        <div className="col-xs-12">
          <div className={styles.tableContainer}>{renderTableBody()}</div>
          <br />
          <br />
          <div>{getChildComponent()}</div>
        </div>
      </div>
    </div>
  );
};

export default ViewRows;
