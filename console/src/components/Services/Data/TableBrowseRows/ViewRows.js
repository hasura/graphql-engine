import React, { useState, useEffect } from 'react';
import 'react-table/react-table.css';
import {
  FaCaretDown,
  FaCaretUp,
  FaClone,
  FaCompress,
  FaEdit,
  FaExpand,
  FaRegCaretSquareRight,
  FaSort,
  FaTrash,
} from 'react-icons/fa';
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

import { Button } from '@/new-components/Button';

import { LegacyRunQueryContainer } from '@/features/BrowseRows';
import { PaginationWithOnlyNavContainer } from '@/new-components/PaginationWithOnlyNav/PaginationWithOnlyNavContainer';

import {
  setOrderCol,
  setOrderType,
  removeOrder,
  runQuery,
  setOffset,
  addOrder,
} from './FilterActions';

import _push from '../push';
import { ordinalColSort } from '../utils';
import Spinner from '../../../Common/Spinner/Spinner';

import { E_SET_EDITITEM } from '../TableEditItem/EditActions';
import { I_SET_CLONE } from '../TableInsertItem/InsertActions';
import {
  getTableInsertRowRoute,
  getTableEditRowRoute,
} from '../../../Common/utils/routesUtils';
import {
  findTable,
  getRelationshipRefTable,
  dataSource,
  getTableCustomColumnName,
  isFeatureSupported,
  currentDriver,
} from '../../../../dataSources';
import { updateSchemaInfo } from '../DataActions';
import {
  persistColumnCollapseChange,
  getPersistedCollapsedColumns,
  persistColumnOrderChange,
  getPersistedColumnsOrder,
} from './tableUtils';
import { compareRows, isTableWithPK } from './utils';

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
      width: 182,
    });

    _gridHeadings.push({
      Header: (
        <div className="flex w-full justify-center items-center">
          <input
            checked={
              curRows.length > 0 && selectedRows.length === curRows.length
            }
            disabled={
              _disableBulkSelect ||
              !isFeatureSupported('tables.browse.bulkRowSelect')
            }
            title={
              _disableBulkSelect
                ? 'No primary key to identify row'
                : 'feature not supported'
            }
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

      let sortIcon = <FaSort />;
      if (curQuery.order_by && curQuery.order_by.length) {
        curQuery.order_by.forEach(orderBy => {
          if (orderBy.column === col.id) {
            sortIcon = orderBy.type === 'asc' ? <FaCaretUp /> : <FaCaretDown />;
          }
        });
      }

      _gridHeadings.push({
        Header: (
          <div className="flex">
            <div>{columnName}</div>
            <div className="absolute right-2.5">{sortIcon}</div>
          </div>
        ),
        accessor: columnName,
        id: col.id,
        foldable: true,
        width: getColWidth(col.id, curRows),
      });
    });

    _relationships.map(rel => {
      const relName = rel.rel_name;

      _gridHeadings.push({
        Header: (
          <div>
            <div>{relName}</div>
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

  const tableSchema = schemas.find(
    x => x.table_name === curTableName && x.table_schema === currentSchema
  );

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
          requirePK = false,
          featureSupported = true
        ) => {
          const disabled = requirePK && !_hasPrimaryKey;

          const disabledOnClick = e => {
            e.preventDefault();
            e.stopPropagation();
          };

          const message = () => {
            if (disabled) {
              return 'No primary key to identify row';
            } else if (!featureSupported) {
              return 'feature not supported';
            }
            return title;
          };

          return (
            <Button
              size="sm"
              icon={icon}
              className="mr-1"
              title={message()}
              disabled={disabled || !featureSupported}
              onClick={
                disabled || !featureSupported ? disabledOnClick : handleClick
              }
              data-test={`row-${type}-button-${rowIndex}`}
            />
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
            icon = <FaCompress />;
            title = 'Collapse row';
            handleClick = handleCollapse;
          } else {
            icon = <FaExpand />;
            title = 'Expand row';
            handleClick = handleExpand;
          }

          return getActionButton('expand', icon, title, handleClick);
        };

        const getEditButton = pkClause => {
          const editIcon = <FaEdit />;

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
            true,
            isFeatureSupported('tables.browse.editRow')
          );
        };

        const getDeleteButton = pkClause => {
          const deleteIcon = <FaTrash />;

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
            true,
            isFeatureSupported('tables.browse.deleteRow')
          );
        };

        const getCloneButton = () => {
          const cloneIcon = <FaClone />;

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
                    size="sm"
                    className="mr-1"
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

          const triggerIcon = <FaRegCaretSquareRight />;
          const triggerTitle = 'Invoke event trigger';

          return (
            <div className="inline">
              <Dropdown
                testId={`data_browse_rows_trigger_${rowIndex}`}
                options={triggerOptions}
                position="right"
                keyPrefix={`invoke_data_dropdown_${rowIndex}`}
              >
                {({ onClick }) =>
                  getActionButton('trigger', triggerIcon, triggerTitle, onClick)
                }
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
            className="flex w-full justify-center items-center !overflow-visible"
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
        <div className="flex w-full justify-center items-center">
          <input
            type="checkbox"
            disabled={
              _disableBulkSelect ||
              !isFeatureSupported('tables.browse.bulkRowSelect')
            }
            title={
              _disableBulkSelect ? NO_PRIMARY_KEY_MSG : 'feature not supported'
            }
            checked={selectedRows.some(selectedRow =>
              compareRows(selectedRow, row, _tableSchema, isView)
            )}
            onChange={e => handleCheckboxChange(row, e, _tableSchema, isView)}
            data-test={`row-checkbox-${rowIndex}`}
          />
        </div>
      );

      // Insert column cells
      _tableSchema.columns
        .map(col => {
          const customColumnName = getTableCustomColumnName(
            tableSchema,
            col.column_name
          );
          if (customColumnName) {
            return {
              ...col,
              column_name: customColumnName,
              id: col?.column_name,
            };
          }
          return { ...col, id: col.column_name };
        })
        .forEach(col => {
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
            const rowColumnValue = row[col.id];

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
                className={isExpanded ? 'whitespace-pre-wrap' : ''}
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
              'text-red-500',
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
  const tableColumnsSorted = tableSchema.columns
    .map(col => {
      const customColumnName = getTableCustomColumnName(
        tableSchema,
        col.column_name
      );
      if (customColumnName) {
        return {
          ...col,
          column_name: customColumnName,
          id: col.column_name,
        };
      }
      return { ...col, id: col.column_name };
    })
    .sort(ordinalColSort);

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
        <div className="flex mb-sm">
          <b className="pr-xs">Selected:</b>
          {selectedRows.length}
          <Button
            icon={<FaTrash />}
            title="Delete selected rows"
            className="ml-xs text-base bg-white border rounded-sm border-gray-400 px-1"
            data-test="bulk-delete"
            onClick={handleDeleteItems}
          />
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
          <ul>{childTabs}</ul>
          {childViewRows}
        </div>
      );
    }

    return _childComponent;
  };

  const [userQuery, setUserQuery] = useState({
    where: { $and: [] },
    order_by: [],
  });

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
      const columnNames = tableColumnsSorted.map(column => column.id);
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
        setSelectedRows([]);
      }
    };

    const handlePageSizeChange = size => {
      if (curFilter.size !== size) {
        setSelectedRows([]);
      }
    };

    const paginationProps = {};
    if (useCustomPagination) {
      paginationProps.PaginationComponent = () => (
        <PaginationWithOnlyNavContainer
          limit={curFilter.limit}
          offset={curFilter.offset}
          onChangePage={handlePageChange}
          onChangePageSize={handlePageSizeChange}
          pageSize={curFilter.size}
          rows={curRows}
          tableSchema={tableSchema}
          userQuery={userQuery}
        />
      );
    }

    return (
      <DragFoldTable
        data={_gridRows}
        columns={_gridHeadings}
        headerTitle={'Click to sort / Drag to rearrange'}
        resizable
        manual
        sortable={false}
        minRows={0}
        getTheadThProps={getTheadThProps}
        getResizerProps={getResizerProps}
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
        showPagination={!shouldHidePagination || useCustomPagination}
        {...paginationProps}
      />
    );
  };

  // Is this ViewRows visible
  let isVisible = false;
  if (!curRelName || curRelName === activePath[curDepth]) {
    isVisible = true;
  }

  const isFilterSectionVisible =
    !isSingleRow && (curRelName === activePath[curDepth] || curDepth === 0);

  const schemaKey = currentDriver === 'bigquery' ? 'dataset' : 'schema';

  return (
    <div className={isVisible ? '' : 'hide '}>
      {isFilterSectionVisible && (
        <div className="mt-4">
          <LegacyRunQueryContainer
            dataSourceName={currentSource}
            table={{
              [schemaKey]: tableSchema.table_schema,
              name: curTableName,
            }}
            onRunQuery={newUserQuery => setUserQuery(newUserQuery)}
          />
        </div>
      )}
      <div className="w-fit ml-0 mt-md">
        {getSelectedRowsSection()}
        <div>
          <div>{renderTableBody()}</div>
          <br />
          <br />
          <div>{getChildComponent()}</div>
        </div>
      </div>
    </div>
  );
};

export default ViewRows;
