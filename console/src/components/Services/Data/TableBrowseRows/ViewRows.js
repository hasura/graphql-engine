import React from 'react';
import ReactTable from 'react-table';
import { Link } from 'react-router';
import 'react-table/react-table.css';
import {
  vExpandRel,
  vCloseRel,
  V_SET_ACTIVE,
  deleteItem,
  vExpandRow,
  vCollapseRow,
} from './ViewActions'; // eslint-disable-line no-unused-vars
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
import { E_SET_EDITITEM } from './EditActions';
import { I_SET_CLONE } from '../TableInsertItem/InsertActions';
import _push from '../push';
import { ordinalColSort, findTableFromRel } from '../utils';
import Spinner from '../../../Common/Spinner/Spinner';
import Button from '../../Layout/Button/Button';
import './ReactTableFix.css';

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
  const tableSchema = schemas.find(x => x.table_name === curTableName);
  const parentTableSchema = parentTableName
    ? schemas.find(t => t.table_name === parentTableName)
    : null;
  const curRelName = curPath.length > 0 ? curPath.slice(-1)[0] : null;

  // Am I a single row display
  let isSingleRow = false;
  if (curQuery.columns.find(c => typeof c === 'object')) {
    // Do I have any children
    isSingleRow = true;
  } else if (curRelName && parentTableSchema) {
    // Am I an obj_rel for my parent?
    if (
      parentTableSchema.relationships.find(
        r => r.name === curRelName && r.rel_type === 'object'
      )
    ) {
      isSingleRow = true;
    }
  }

  // Get the headings
  const tableHeadings = [];
  const gridHeadings = [];
  const sortedColumns = tableSchema.columns.sort(ordinalColSort);

  if (!isView) {
    gridHeadings.push({
      Header: '',
      accessor: 'actions',
    });
  }

  sortedColumns.map((column, i) => {
    tableHeadings.push(<th key={i}>{column.column_name}</th>);
    gridHeadings.push({
      Header: column.column_name,
      accessor: column.column_name,
    });
  });

  tableHeadings.push(
    <th
      key="relIndicator"
      style={{ minWidth: 'auto', color: '#aaa', fontWeight: 300 }}
    >
      {' '}
      &lt;&gt;{' '}
    </th>
  );
  tableSchema.relationships.map((r, i) => {
    tableHeadings.push(
      <th key={tableSchema.columns.length + i}>{r.rel_name}</th>
    );
    gridHeadings.push({
      Header: r.rel_name,
      accessor: r.rel_name,
    });
  });

  const hasPrimaryKeys =
    tableSchema.primary_key && tableSchema.primary_key.columns.length > 0;
  let editButton;
  let cloneButton;
  let deleteButton;

  const newCurRows = [];

  curRows.forEach((row, rowIndex) => {
    const newRow = {};
    const pkClause = {};
    tableSchema.relationships.forEach((r, k) => {
      if (curQuery.columns.find(c => c.name === r.rel_name)) {
        // already expanded

        newRow[r.rel_name] = (
          <div className={styles.tableCellCenterAligned}>
            <a
              href="#"
              className={styles.expanded}
              onClick={e => {
                e.preventDefault();
                dispatch(vCloseRel(curPath, r.rel_name));
              }}
            >
              Close
            </a>
          </div>
        );
      } else {
        // check if it can be expanded
        const currentFkey = r.rel_def.foreign_key_constraint_on;
        const currentFkeyValue = row[currentFkey];
        if (currentFkeyValue === null) {
          // cannot be expanded as value is null
          newRow[r.rel_name] = (
            <div
              className={styles.tableCellCenterAligned}
              key={tableSchema.columns.length + 10 + k}
            >
              <i>NULL</i>
            </div>
          );
        } else {
          // can be expanded
          newRow[r.rel_name] = (
            <div
              className={styles.tableCellCenterAligned}
              key={tableSchema.columns.length + 10 + k}
            >
              <a
                href="#"
                onClick={e => {
                  e.preventDefault();
                  dispatch(vExpandRel(curPath, r.rel_name, pkClause));
                }}
              >
                View
              </a>
            </div>
          );
        }
      }
    });
    if (!isView && hasPrimaryKeys) {
      tableSchema.primary_key.columns.map(pk => {
        pkClause[pk] = row[pk];
      });
    } else {
      tableSchema.columns.map(k => {
        pkClause[k.column_name] = row[k.column_name];
      });
    }
    if (!isSingleRow && !isView && hasPrimaryKeys) {
      editButton = (
        <Button
          className={styles.add_mar_right_small}
          color="white"
          size="xs"
          onClick={() => {
            dispatch({ type: E_SET_EDITITEM, oldItem: row, pkClause });
            dispatch(
              _push(`/schema/${currentSchema}/tables/${curTableName}/edit`)
            );
          }}
          data-test={`row-edit-button-${rowIndex}`}
        >
          Edit
        </Button>
      );

      cloneButton = (
        <Button
          className={styles.add_mar_right_small}
          size="xs"
          color="white"
          onClick={() => {
            dispatch({ type: I_SET_CLONE, clone: row });
            dispatch(
              _push(`/schema/${currentSchema}/tables/${curTableName}/insert`)
            );
          }}
          data-test={`row-clone-button-${rowIndex}`}
        >
          Clone
        </Button>
      );

      deleteButton = (
        <Button
          className={styles.add_mar_right_small}
          size="xs"
          color="white"
          onClick={() => {
            dispatch(deleteItem(pkClause));
          }}
          data-test={`row-delete-button-${rowIndex}`}
        >
          Delete
        </Button>
      );
    }
    const buttonsDiv = (
      <div className={styles.tableCellCenterAligned}>
        {cloneButton}
        {editButton}
        {deleteButton}
      </div>
    );
    // Insert Edit, Delete, Clone in a cell
    newRow.actions = buttonsDiv;
    // Insert cells corresponding to all rows
    tableSchema.columns.forEach(col => {
      if (col.data_type === 'json' || col.data_type === 'jsonb') {
        if (row[col.column_name] === null) {
          newRow[col.column_name] = (
            <div className={styles.tableCellCenterAligned}>
              <i>NULL</i>
            </div>
          );
        } else {
          newRow[col.column_name] = (
            <div className={styles.tableCellCenterAligned}>
              {JSON.stringify(row[col.column_name])}
            </div>
          );
        }
      } else {
        const getCellContent = () => {
          let conditionalClassname = styles.tableCellCenterAligned;
          const cellIndex = `${curTableName}-${col}-${rowIndex}`;
          if (expandedRow === cellIndex) {
            conditionalClassname = styles.tableCellCenterAlignedExpanded;
          }
          if (row[col.column_name] === null) {
            return (
              <div className={conditionalClassname}>
                <i>NULL</i>
              </div>
            );
          }
          const content =
            row[col.column_name] === undefined
              ? 'NULL'
              : row[col.column_name].toString();
          const expandOrCollapseBtn =
            expandedRow === cellIndex ? (
              <i
                className={`${styles.cellCollapse} fa fa-minus`}
                onClick={() => dispatch(vCollapseRow())}
              >
                {' '}
              </i>
            ) : (
              <i
                className={`${styles.cellExpand} fa fa-expand`}
                onClick={() => dispatch(vExpandRow(cellIndex))}
              >
                {' '}
              </i>
            );
          if (content.length > 20) {
            return (
              <div className={conditionalClassname}>
                {expandOrCollapseBtn}
                {content}
              </div>
            );
          }
          return <div className={conditionalClassname}>{content}</div>;
        };
        newRow[col.column_name] = getCellContent();
      }
    });
    newCurRows.push(newRow);
  });
  // If query object has expanded columns
  let childComponent = null;
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
    childComponent = (
      <div>
        <ul className="nav nav-tabs">{childTabs}</ul>
        {childViewRows}
      </div>
    );
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

  // check if no primary key

  const primaryKeyMsg = [];
  if (!hasPrimaryKeys) {
    if (!isView) {
      primaryKeyMsg.push(
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
    dispatch(runQuery(tableSchema));
    // Add a new empty filter
    dispatch(addOrder());
  };

  const changePage = page => {
    if (curFilter.offset !== page * curFilter.limit) {
      dispatch(setOffset(page * curFilter.limit));
      dispatch(runQuery(tableSchema));
    }
  };

  const changePageSize = size => {
    if (curFilter.size !== size) {
      dispatch(setLimit(size));
      dispatch(runQuery(tableSchema));
    }
  };

  const renderTableBody = () => {
    if (isProgressing) {
      return (
        <div>
          {' '}
          <Spinner />{' '}
        </div>
      );
    } else if (count === 0) {
      return <div> No rows found. </div>;
    }
    let shouldSortColumn = true;
    return (
      <ReactTable
        className="-highlight"
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
      />
    );
  };

  return (
    <div className={isVisible ? '' : 'hide '}>
      {filterQuery}
      <hr />
      {primaryKeyMsg}
      <div className="row">
        <div className="col-xs-12">
          <div className={styles.tableContainer}>{renderTableBody()}</div>
          <br />
          <br />
          {childComponent}
        </div>
      </div>
    </div>
  );
};

export default ViewRows;
