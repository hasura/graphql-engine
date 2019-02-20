import React from 'react';
import ReactTable from 'react-table';
import { Link } from 'react-router';
import 'react-table/react-table.css';
import './ReactTableFix.css';

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
import Button from '../../Layout/Button/Button';

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
      });
    }

    _columns.map(col => {
      _gridHeadings.push({
        Header: col.column_name,
        accessor: col.column_name,
      });
    });

    _relationships.map(rel => {
      _gridHeadings.push({
        Header: rel.rel_name,
        accessor: rel.rel_name,
      });
    });

    return _gridHeadings;
  };

  const getGridRows = (_tableSchema, _hasPrimaryKey, _isSingleRow) => {
    const _gridRows = [];

    curRows.forEach((row, rowIndex) => {
      const newRow = {};

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

        const getButton = (type, handleClick) => {
          return (
            <Button
              className={styles.add_mar_right_small}
              color="white"
              size="xs"
              onClick={handleClick}
              data-test={`row-${type.toLowerCase()}-button-${rowIndex}`}
            >
              { type }
            </Button>
          );
        };

        const allowModify = !_isSingleRow && !isView && _hasPrimaryKey;

        if (allowModify) {
          const pkClause = getPKClause();

          const handleEditClick = () => {
            dispatch({ type: E_SET_EDITITEM, oldItem: row, pkClause });
            dispatch(
              _push(`/schema/${currentSchema}/tables/${curTableName}/edit`)
            );
          };

          const handleCloneClick = () => {
            dispatch({ type: I_SET_CLONE, clone: row });
            dispatch(
              _push(`/schema/${currentSchema}/tables/${curTableName}/insert`)
            );
          };

          const handleDeleteClick = () => {
            dispatch(deleteItem(pkClause));
          };

          editButton = getButton('Edit', handleEditClick);
          cloneButton = getButton('Clone', handleCloneClick);
          deleteButton = getButton('Delete', handleDeleteClick);
        }

        return (
          <div className={styles.tableCellCenterAligned}>
            {cloneButton}
            {editButton}
            {deleteButton}
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

          const cellIndex = `${curTableName}-${col}-${rowIndex}`;
          const isExpanded = expandedRow === cellIndex;

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

          const getCellExpander = (cellValue) => {
            const cellCapacity = 15; // depends on smallest possible cell size
            const needsExpander = (typeof cellValue === 'string') && cellValue.length > cellCapacity;

            let expandOrCollapseBtn = '';
            if (needsExpander) {
              const handleExpand = () => dispatch(vExpandRow(cellIndex));
              const handleCollapse = () => dispatch(vCollapseRow());

              expandOrCollapseBtn = (
                <i
                  className={`${styles.cellExpander} fa ${isExpanded ? 'fa-compress' : 'fa-expand'}`}
                  onClick={isExpanded ? handleCollapse : handleExpand}
                />
              );
            }

            return expandOrCollapseBtn;
          };

          const cellValue = getCellValue();

          const cellExpander = getCellExpander(cellValue);

          return (
            <div className={isExpanded ? styles.tableCellExpanded : ''}>
              {cellExpander} {cellValue}
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

    const sortByColumn = col => {
      const columnNames = tableColumnsSorted.map(column => column.column_name);
      if (!columnNames.includes(col)) {
        return;
      }

      // Remove all the existing order_bys
      const numOfOrderBys = curFilter.order_by.length;
      for (let i = 0; i < numOfOrderBys - 1; i++) {
        dispatch(removeOrder(1));
      }

      let orderType;
      if (
        curFilter.order_by.length !== 0 &&
        curFilter.order_by[0].column === col &&
        curFilter.order_by[0].type === 'asc'
      ) {
        orderType = 'desc';
      } else {
        orderType = 'asc';
      }

      // Go back to the first page
      dispatch(setOffset(0));
      // Set the filter
      dispatch(setOrderCol(col, 0));
      dispatch(setOrderType(orderType, 0));
      // Run query
      dispatch(runQuery(tableSchema));
      // Add a new empty filter
      dispatch(addOrder());
    };

    const getTheadThProps = (finalState, some, column) => ({
      onClick: () => {
        if (
          !disableSortColumn &&
          column.Header
        ) {
          sortByColumn(column.Header);
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
      <ReactTable
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
