import React from 'react';
import 'react-table/react-table.css';
import '../../../Common/TableCommon/ReactTableOverrides.css';
import DragFoldTable from '../../../Common/TableCommon/DragFoldTable';

import Dropdown from '../../../Common/Dropdown/Dropdown';

import InvokeManualTrigger from '../../EventTrigger/Common/InvokeManualTrigger/InvokeManualTrigger';

import {
  vExpandRel,
  vCloseRel,
  V_SET_ACTIVE,
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
import { ordinalColSort, findTableFromRel } from '../utils';
import FilterQuery from './FilterQuery';
import Spinner from '../../../Common/Spinner/Spinner';
import Button from '../../../Common/Button/Button';

import { E_SET_EDITITEM } from './EditActions';
import { I_SET_CLONE } from '../TableInsertItem/InsertActions';

const ViewRows = ({
  curTableName,
  currentSchema,
  curQuery,
  curFilter,
  curRows,
  curPath,
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
  updateInvocationRow,
  updateInvocationFunction,
  triggeredRow,
  triggeredFunction,
}) => {
  const styles = require('../../../Common/TableCommon/Table.scss');

  // Invoke manual trigger status
  const invokeTrigger = (trigger, row) => {
    updateInvocationRow(row);
    updateInvocationFunction(trigger);
  };

  const onCloseInvokeTrigger = () => {
    updateInvocationRow(-1);
    updateInvocationFunction(null);
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
        r => r.name === _curRelName && r.rel_type === 'object'
      )
    ) {
      // Am I an obj_rel for my parent?
      _isSingleRow = true;
    }

    return _isSingleRow;
  };

  const checkIfHasPrimaryKey = _tableSchema => {
    return (
      _tableSchema.primary_key && _tableSchema.primary_key.columns.length > 0
    );
  };

  const getGridHeadings = (_columns, _relationships) => {
    const _gridHeadings = [];

    _gridHeadings.push({
      Header: '',
      accessor: 'tableRowActionButtons',
      id: 'tableRowActionButtons',
    });

    _columns.map(col => {
      const columnName = col.column_name;

      let sortIcon = 'fa-sort';
      if (curQuery.order_by && curQuery.order_by.length) {
        sortIcon = '';

        curQuery.order_by.forEach(orderBy => {
          if (orderBy.column === columnName) {
            sortIcon = orderBy.type === 'asc' ? 'fa-caret-up' : 'fa-caret-down';
          }
        });
      }

      _gridHeadings.push({
        Header: (
          <div className="ellipsis" title="Click to sort">
            <span className={styles.tableHeaderCell}>
              {columnName} <i className={'fa ' + sortIcon} />
            </span>
          </div>
        ),
        accessor: columnName,
        id: columnName,
        foldable: true,
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
      });
    });

    return _gridHeadings;
  };

  const getGridRows = (_tableSchema, _hasPrimaryKey, _isSingleRow) => {
    const _gridRows = [];

    curRows.forEach((row, rowIndex) => {
      const newRow = {};

      const rowCellIndex = `${curTableName}-${rowIndex}`;
      const isExpanded = expandedRow === rowCellIndex;

      const getPKClause = () => {
        const pkClause = {};

        if (!isView && _hasPrimaryKey) {
          _tableSchema.primary_key.columns.map(pk => {
            pkClause[pk] = row[pk];
          });
        } else {
          _tableSchema.columns.map(k => {
            pkClause[k.column_name] = row[k.column_name];
          });
        }

        return pkClause;
      };

      const getActionButtons = () => {
        let editButton;
        let cloneButton;
        let deleteButton;
        let expandButton;
        let manualTriggersButton;

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

          const handleExpand = () => dispatch(vExpandRow(rowCellIndex));
          const handleCollapse = () => dispatch(vCollapseRow());

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
              _push(`/schema/${currentSchema}/tables/${curTableName}/edit`)
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
            dispatch(deleteItem(pkClause));
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
              _push(`/schema/${currentSchema}/tables/${curTableName}/insert`)
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
              callbackArguments: [m.name, rowIndex],
              buttonText: 'Invoke',
              displayText: m.name,
              testId: m.name,
              onClick: invokeTrigger,
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

          const invokeManualTrigger = r =>
            triggeredRow === rowIndex && (
              <InvokeManualTrigger
                args={r}
                name={`${triggeredFunction}`}
                onClose={onCloseInvokeTrigger}
                key={`invoke_function_${triggeredFunction}`}
                identifier={`invoke_function_${triggeredFunction}`}
              />
            );

          return (
            <div className={styles.display_inline}>
              <Dropdown
                testId={`data_browse_rows_trigger_${rowIndex}`}
                options={triggerOptions}
                position="right"
                key={`invoke_data_dropdown_${rowIndex}`}
                keyPrefix={`invoke_data_dropdown_${rowIndex}`}
              >
                {triggerBtn}
              </Dropdown>
              {invokeManualTrigger(row)}
            </div>
          );
        };

        const showActionBtns = !_isSingleRow && !isView;

        if (showActionBtns) {
          const pkClause = getPKClause();

          editButton = getEditButton(pkClause);
          deleteButton = getDeleteButton(pkClause);
          cloneButton = getCloneButton();
        }

        // eslint-disable-next-line prefer-const
        expandButton = getExpandButton();
        // eslint-disable-next-line prefer-const
        manualTriggersButton = getManualTriggersButton();

        return (
          <div key={rowIndex} className={styles.tableCellCenterAligned}>
            {cloneButton}
            {editButton}
            {deleteButton}
            {expandButton}
            {manualTriggersButton}
          </div>
        );
      };

      // Insert Edit, Delete, Clone in a cell
      newRow.tableRowActionButtons = getActionButtons();

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
          } else if (col.data_type === 'json' || col.data_type === 'jsonb') {
            cellValue = JSON.stringify(rowColumnValue);
            cellTitle = cellValue;
          } else {
            /*
             * This will render [object Object] if the state is not common data types
             * */
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
              const pkClause = getPKClause();

              const handleViewClick = e => {
                e.preventDefault();
                dispatch(vExpandRel(curPath, rel.rel_name, pkClause));
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

  const hasPrimaryKey = checkIfHasPrimaryKey(tableSchema);

  const isSingleRow = checkIfSingleRow(curRelName);

  const _gridHeadings = getGridHeadings(tableColumnsSorted, tableRelationships);

  const _gridRows = getGridRows(tableSchema, hasPrimaryKey, isSingleRow);

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
        const limit = 'limit' in curFilter ? curFilter.limit : 10;
        const offset = 'offset' in curFilter ? curFilter.offset : 0;

        _filterQuery = (
          <FilterQuery
            curQuery={curQuery}
            whereAnd={wheres}
            tableSchema={tableSchema}
            orderBy={orderBy}
            limit={limit}
            dispatch={dispatch}
            count={count}
            tableName={curTableName}
            offset={offset}
          />
        );
      }
    }

    return _filterQuery;
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
      if (curRows[0][cq.name]) {
        const rel = tableSchema.relationships.find(r => r.rel_name === cq.name);

        if (rel) {
          let childRows = curRows[0][cq.name];
          if (rel.rel_type === 'object') {
            childRows = [childRows];
          }
          // Find the name of this childTable using the rel
          return (
            <ViewRows
              key={i}
              curTableName={
                findTableFromRel(schemas, tableSchema, rel).table_name
              }
              currentSchema={currentSchema}
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
      }
    };

    const handlePageSizeChange = size => {
      if (curFilter.size !== size) {
        dispatch(setLimit(size));
        dispatch(setOffset(0));
        dispatch(runQuery(tableSchema));
      }
    };

    return (
      <DragFoldTable
        className="-highlight"
        data={_gridRows}
        columns={_gridHeadings}
        resizable
        manual
        sortable={false}
        minRows={0}
        getTheadThProps={getTheadThProps}
        getResizerProps={getResizerProps}
        showPagination={count > curFilter.limit}
        defaultPageSize={Math.min(curFilter.limit, count)}
        pages={Math.ceil(count / curFilter.limit)}
        onPageChange={handlePageChange}
        onPageSizeChange={handlePageSizeChange}
        page={Math.floor(curFilter.offset / curFilter.limit)}
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
      <hr />
      <div className="row">
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
