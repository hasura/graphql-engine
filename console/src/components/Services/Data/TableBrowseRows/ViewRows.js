import React from 'react';
import { Link } from 'react-router';
import 'react-table/react-table.css';
import './ReactTableFix.css';
import DragFoldTable from './DragFoldTable';

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
}) => {
  const styles = require('../TableCommon/Table.scss');

  const checkIfSingleRow = (_curRelName) => {
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
      parentTableSchema.relationships.find(r => r.name === _curRelName && r.rel_type === 'object')
    ) {
      // Am I an obj_rel for my parent?
      _isSingleRow = true;
    }

    return _isSingleRow;
  };

  const checkIfHasPrimaryKey = (_tableSchema) => {
    return _tableSchema.primary_key && _tableSchema.primary_key.columns.length > 0;
  };

  const getGridHeadings = (_columns, _relationships) => {
    const _gridHeadings = [];

    if (!isView) {
      _gridHeadings.push({
        Header: '',
        accessor: 'actions',
        id: 'actions',
      });
    }

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
              { columnName } <i className={'fa ' + sortIcon} />
            </span>
          </div>
        ),
        accessor: columnName,
        id: columnName,
        foldable: true
      });
    });

    _relationships.map(rel => {
      const relName = rel.rel_name;

      _gridHeadings.push({
        Header: (
          <div className="ellipsis">
            <span className={styles.tableHeaderCell}>
              { relName }
            </span>
          </div>
        ),
        accessor: relName,
        id: relName,
        foldable: true
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

      const getButtons = () => {
        let editButton;
        let cloneButton;
        let deleteButton;
        let expandButton;

        const getButton = (type, icon, title, handleClick) => {
          return (
            <Button
              className={styles.add_mar_right_small}
              color="white"
              size="xs"
              onClick={handleClick}
              title={title}
              data-test={`row-${type}-button-${rowIndex}`}
            >
              { icon }
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

          const expanderIcon = (
            <i className={`fa ${icon}`} />
          );

          return getButton('expand', expanderIcon, title, handleClick);
        };

        const getEditButton = (pkClause) => {
          const editIcon = (
            <i className="fa fa-edit" />
          );

          const handleEditClick = () => {
            dispatch({ type: E_SET_EDITITEM, oldItem: row, pkClause });
            dispatch(
              _push(`/schema/${currentSchema}/tables/${curTableName}/edit`)
            );
          };

          const editTitle = 'Edit row';

          return getButton('edit', editIcon, editTitle, handleEditClick);
        };

        const getDeleteButton = (pkClause) => {
          const deleteIcon = (
            <i className="fa fa-trash" />
          );

          const handleDeleteClick = () => {
            dispatch(deleteItem(pkClause));
          };

          const deleteTitle = 'Delete row';

          return getButton('delete', deleteIcon, deleteTitle, handleDeleteClick);
        };

        const getCloneButton = () => {
          const cloneIcon = (
            <i className="fa fa-clone" />
          );

          const handleCloneClick = () => {
            dispatch({ type: I_SET_CLONE, clone: row });
            dispatch(
              _push(`/schema/${currentSchema}/tables/${curTableName}/insert`)
            );
          };

          const cloneTitle = 'Clone row';

          return getButton('clone', cloneIcon, cloneTitle, handleCloneClick);
        };

        const allowModify = !_isSingleRow && !isView && _hasPrimaryKey;

        if (allowModify) {
          const pkClause = getPKClause();

          editButton = getEditButton(pkClause);
          deleteButton = getDeleteButton(pkClause);
          cloneButton = getCloneButton();
        }

        // eslint-disable-next-line prefer-const
        expandButton = getExpandButton();

        return (
          <div className={styles.tableCellCenterAligned}>
            {cloneButton}
            {editButton}
            {deleteButton}
            {expandButton}
          </div>
        );
      };

      // Insert Edit, Delete, Clone in a cell
      newRow.actions = getButtons();

      // Insert column cells
      _tableSchema.columns.forEach(col => {
        const columnName = col.column_name;

        const getColCellContent = () => {
          const rowColumnValue = row[columnName];

          const getCellValue = () => {
            let cellValue = '';

            if (rowColumnValue === null) {
              cellValue = (<i>NULL</i>);
            } else if (rowColumnValue === undefined) {
              cellValue = 'NULL';
            } else if (col.data_type === 'json' || col.data_type === 'jsonb') {
              cellValue = JSON.stringify(rowColumnValue);
            } else {
              cellValue = rowColumnValue.toString();
            }

            return cellValue;
          };

          const cellValue = getCellValue();

          return (
            <div className={isExpanded ? styles.tableCellExpanded : ''}>
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
              <a
                href="#"
                className={className}
                onClick={clickHandler}
              >
                {value}
              </a>
            );
          };

          const isExpanded = curQuery.columns.find(c => c.name === rel.rel_name) !== undefined;

          if (isExpanded) {
            const handleCloseClick = e => {
              e.preventDefault();
              dispatch(vCloseRel(curPath, rel.rel_name));
            };

            cellValue = getRelExpander('Close', styles.expanded, handleCloseClick);
          } else {
            const currentFkey = rel.rel_def.foreign_key_constraint_on;
            const currentFkeyValue = row[currentFkey];

            if (currentFkeyValue === null) {
              // cannot be expanded as value is null
              cellValue = (
                <i>NULL</i>
              );
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

          return (
            <div>
              { cellValue }
            </div>
          );
        };

        newRow[relName] = getRelCellContent();
      });

      _gridRows.push(newRow);
    });

    return _gridRows;
  };

  const curRelName = curPath.length > 0 ? curPath.slice(-1)[0] : null;
  const tableSchema = schemas.find(x => x.table_name === curTableName);

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

  // If no primary key
  const getPrimaryKeyMsg = () => {
    const _primaryKeyMsg = [];

    if (!hasPrimaryKey) {
      if (!isView) {
        _primaryKeyMsg.push(
          <div key="primaryKeyMsg" className="row">
            <div className="col-xs-12">
              <div className="alert alert-warning" role="alert">
                There is no unique identifier (primary Key) for a row. You need
                at-least one primary key to allow editing, Please use{' '}
                <Link to="/data/sql">Raw SQL</Link> to make one or more column as
                primary key.
              </div>
            </div>
          </div>
        );
      }
    }
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
        let childRows = curRows[0][cq.name];
        if (rel.rel_type === 'object') {
          childRows = [childRows];
        }
        // Find the name of this childTable using the rel
        return (
          <ViewRows
            key={i}
            curTableName={findTableFromRel(schemas, tableSchema, rel).table_name}
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
          />
        );
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
          {' '}<Spinner />{' '}
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
      onClick: (e) => {
        if (
          !disableSortColumn &&
          column.id
        ) {
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
      { getFilterQuery() }
      <hr />
      { getPrimaryKeyMsg() }
      <div className="row">
        <div className="col-xs-12">
          <div className={styles.tableContainer}>
            { renderTableBody() }
          </div>
          <br />
          <br />
          <div>
            { getChildComponent() }
          </div>
        </div>
      </div>
    </div>
  );
};

export default ViewRows;
