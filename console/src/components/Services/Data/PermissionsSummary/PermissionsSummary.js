import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import Modal from '../../../Common/Modal/Modal';
import Button from '../../../Common/Button/Button';

import styles from './PermissionsSummary.scss';

import { getTablePermissionsRoute } from '../../../Common/utils/routesUtils';
import { permissionsSymbols } from '../../../Common/Permissions/PermissionSymbols';
import {
  findTable,
  getTableNameWithSchema,
  getTableDef,
  getSchemaTables,
  getTrackedTables,
  dataSource,
} from '../../../../dataSources';
import { getConfirmation } from '../../../Common/utils/jsUtils';

import { updateSchemaInfo } from '../DataActions';
import {
  copyRolePermissions,
  permOpenEdit,
  deleteRoleGlobally,
} from '../TablePermissions/Actions';

import {
  getAllRoles,
  getPermissionFilterString,
  getPermissionColumnAccessSummary,
  getTablePermissionsByRoles,
  getPermissionRowAccessSummary,
} from './utils';

import Header from './Header';
import RolesHeader from './RolesHeader';
import { RightContainer } from '../../../Common/Layout/RightContainer';

class PermissionsSummary extends Component {
  initState = {
    currRole: null,
    currTable: null,
    currAction: 'select',
    copyState: {
      copyFromRole: '',
      copyFromTable: '',
      copyFromAction: '',
      copyToRoles: [],
      newRole: '',
    },
  };

  constructor(props) {
    super(props);

    this.state = {
      ...this.initState,
      copyState: {
        ...this.initState.copyState,
      },
    };

    this.props.dispatch(
      updateSchemaInfo({ schemas: [this.props.currentSchema] })
    );
  }

  render() {
    const { currRole, currAction, currTable, copyState } = this.state;
    const { dispatch, currentSource } = this.props;

    // ------------------------------------------------------------------------------

    const { allSchemas, currentSchema } = this.props;

    const currSchemaTrackedTables = getTrackedTables(
      getSchemaTables(allSchemas, currentSchema)
    );

    const allActions = ['select', 'insert', 'update', 'delete'];

    let allRoles = getAllRoles(allSchemas);

    // add newly added roles
    const newRoles = copyState.copyToRoles.filter(r => !allRoles.includes(r));
    allRoles = allRoles.concat(newRoles);

    // ------------------------------------------------------------------------------

    const noAccessDisplay = (
      <div className={styles.text_center + ' ' + styles.wd100}>
        {permissionsSymbols.noAccess}
      </div>
    );

    const fullAccessDisplay = (
      <div className={styles.text_center + ' ' + styles.wd100}>
        {permissionsSymbols.fullAccess}
      </div>
    );

    // ------------------------------------------------------------------------------

    const getActionSelector = () => {
      const setAction = e => {
        this.setState({ currAction: e.target.value });
      };

      const getActionsOptions = () => {
        return allActions.map(action => (
          <option key={action} value={action}>
            {action}
          </option>
        ));
      };

      return (
        <th key={'action-select'} className={styles.selected}>
          <select
            value={currAction}
            className={'form-control'}
            onChange={setAction}
          >
            {getActionsOptions()}
          </select>
        </th>
      );
    };

    const getBackBtn = dimension => {
      const clear = e => {
        e.preventDefault();

        this.setState({ [dimension]: null });
      };

      return (
        <th
          key={'back-btn'}
          className={styles.cursorPointer + ' ' + styles.selected}
          onClick={clear}
        >
          <span className={styles.text_link}>&larr; Back</span>
        </th>
      );
    };

    const getActionIcon = (faIconType, onClick = null) => {
      return (
        <i
          className={`fa ${faIconType} ${styles.actionIcon}`}
          aria-hidden="true"
          onClick={onClick}
        />
      );
    };

    const getEditIcon = () => {
      return getActionIcon('fa-pencil');
    };

    // ------------------------------------------------------------------------------

    const getClickableCell = (
      key,
      onClick,
      content,
      actionIcon,
      cellTooltip
    ) => {
      return (
        <td
          key={key}
          title={cellTooltip}
          className={styles.clickableCell}
          onClick={onClick}
        >
          <div
            className={styles.display_flex + ' ' + styles.flex_space_between}
          >
            <div>{content}</div>
            <div>{actionIcon}</div>
          </div>
        </td>
      );
    };

    const getCellOnClick = (table, role, action) => {
      return () => {
        dispatch(
          push(
            getTablePermissionsRoute(
              table.table_schema,
              currentSource,
              table.table_name,
              dataSource.isTable(table)
            )
          )
        );

        if (role && action) {
          // TODO: fix this. above redirect clears state set by this
          dispatch(permOpenEdit(table, role, action));
        }
      };
    };

    const copyOnClick = (e, role) => {
      e.preventDefault();
      e.stopPropagation();

      this.setState({
        copyState: {
          ...copyState,
          copyFromRole: role,
          copyFromTable: currTable ? getTableNameWithSchema(currTable) : 'all',
          copyFromAction: currRole ? 'all' : currAction,
        },
      });
    };

    const deleteOnClick = (e, role) => {
      e.preventDefault();
      e.stopPropagation();

      const deleteConfirmed = getConfirmation(
        `This will delete all permissions for the role: "${role}" for all entities in the current Postgres schema`,
        true,
        role
      );

      if (deleteConfirmed) {
        dispatch(deleteRoleGlobally(role));
      }
    };

    const setRole = (role, isCurrRole) => {
      this.setState({ currRole: isCurrRole ? null : role });
      window.scrollTo(0, 0);
    };

    const defaultRolesHeaderProps = {
      allRoles,
      currentRole: currRole,
      onCopyClick: copyOnClick,
      onDeleteClick: deleteOnClick,
      setRole,
    };

    const getRolesCells = (table, roleCellRenderer) => {
      const tablePermissions = getTablePermissionsByRoles(table);

      return allRoles.map(role => {
        const rolePermissions = tablePermissions[role];
        const actionPermission = rolePermissions
          ? rolePermissions[currAction]
          : null;

        const cellKey = role;
        const cellContent = roleCellRenderer(actionPermission, table);
        const editIcon = getEditIcon();
        const cellOnClick = getCellOnClick(table, role, currAction);
        const cellTooltip = 'Go to edit page';

        return getClickableCell(
          cellKey,
          cellOnClick,
          cellContent,
          editIcon,
          cellTooltip
        );
      });
    };

    const getActionsCells = (table, actionCellRenderer) => {
      const tablePermissions = getTablePermissionsByRoles(table);
      const rolePermission = tablePermissions[currRole] || {};

      return allActions.map(action => {
        const actionPermission = rolePermission[action];

        const cellKey = action;
        const cellContent = actionCellRenderer(actionPermission, table, action);
        const editIcon = getEditIcon();
        const cellOnClick = getCellOnClick(table, currRole, action);
        const cellTooltip = 'Go to edit page';

        return getClickableCell(
          cellKey,
          cellOnClick,
          cellContent,
          editIcon,
          cellTooltip
        );
      });
    };

    const getTablesRows = (
      tableRowCellRenderer,
      selectable = true,
      selectedFirst = false
    ) => {
      const tablesRows = [];

      if (!currSchemaTrackedTables.length) {
        tablesRows.push(
          <tr key={'No tables'}>
            <Header content="No tables" selectable={false} />
          </tr>
        );
      } else {
        currSchemaTrackedTables.forEach((table, i) => {
          const tableName = table.table_name;
          const tableSchema = table.table_schema;

          const isCurrTable =
            currTable &&
            currTable.name === tableName &&
            currTable.schema === tableSchema;

          const getTableHeader = () => {
            const setTable = () => {
              this.setState({
                currTable: isCurrTable ? null : getTableDef(table),
              });
            };

            return (
              <Header
                content={dataSource.displayTableName(table)}
                selectable={selectable}
                isSelected={isCurrTable}
                onClick={setTable}
                actionButtons={[]}
                key={tableName}
              />
            );
          };

          const tableRow = (
            <tr key={i}>
              {getTableHeader()}
              {tableRowCellRenderer(table)}
            </tr>
          );

          if (selectedFirst && isCurrTable) {
            tablesRows.unshift(tableRow);
          } else {
            tablesRows.push(tableRow);
          }
        });
      }

      return tablesRows;
    };

    // ------------------------------------------------------------------------------

    const rolePermissionsRenderer = (
      actionPermission,
      table,
      action = currAction,
      showDetails = false
    ) => {
      let permissionDisplay;

      if (!actionPermission) {
        permissionDisplay = noAccessDisplay;
      } else {
        const getRowsDisplay = () => {
          let filterDisplay = noAccessDisplay;

          const filterString = getPermissionFilterString(
            actionPermission,
            action,
            true
          );

          if (filterString) {
            const getRowsDetails = () => {
              return <pre>{filterString}</pre>;
            };

            filterDisplay = (
              <div>
                <b>Rows</b> -{' '}
                <i>{getPermissionRowAccessSummary(filterString)}</i>
                {showDetails && getRowsDetails()}
              </div>
            );
          }

          return filterDisplay;
        };

        const getColumnsDisplay = () => {
          let columnsDisplay = null;

          const columns = actionPermission.columns;
          if (columns) {
            const getColumnsDetails = () => {
              return <div>{columns.length ? columns.join(', ') : 'None'}</div>;
            };

            columnsDisplay = (
              <div className={styles.add_mar_bottom_small}>
                <b>Columns</b> -{' '}
                <i>
                  {getPermissionColumnAccessSummary(actionPermission, {
                    columns: table.columns,
                  })}
                </i>
                {showDetails && getColumnsDetails()}
              </div>
            );
          }

          return columnsDisplay;
        };

        permissionDisplay = (
          <div>
            {getRowsDisplay()}
            {getColumnsDisplay()}
          </div>
        );
      }

      return permissionDisplay;
    };

    // ------------------------------------------------------------------------------

    const getTableAllRolesTable = () => {
      const currTableInfo = findTable(currSchemaTrackedTables, currTable);

      const getTablesColumnTable = () => {
        return (
          <table
            className={`table table-bordered ${styles.rolesTable} ${styles.remove_margin}`}
          >
            <thead>
              <tr>{getBackBtn('currTable')}</tr>
            </thead>
            <tbody>{getTablesRows(() => {}, true, true)}</tbody>
          </table>
        );
      };

      const getTableAllRolesAllActionsTable = () => {
        const getRowRows = () => {
          const rowRows = [];

          const getTableActionRolesRowPermissions = () => {
            const roleRowPermissionsRenderer = actionPermission => {
              let rowsDisplay = noAccessDisplay;

              const filterString = getPermissionFilterString(
                actionPermission,
                currAction,
                true
              );

              if (filterString) {
                const getRowsDetails = () => {
                  return <pre>{filterString}</pre>;
                };

                rowsDisplay = (
                  <div>
                    {getPermissionRowAccessSummary(filterString)}
                    {getRowsDetails()}
                  </div>
                );
              }

              return rowsDisplay;
            };

            return getRolesCells(currTableInfo, roleRowPermissionsRenderer);
          };

          const getTableActionRolesRowLimits = () => {
            const roleRowLimitRenderer = actionPermission => {
              return (
                <div className={styles.text_center}>
                  {actionPermission && actionPermission.limit !== undefined
                    ? actionPermission.limit
                    : noAccessDisplay}
                </div>
              );
            };

            return getRolesCells(currTableInfo, roleRowLimitRenderer);
          };

          const getTableActionRolesAggregationAllowed = () => {
            const roleAggregateAllowedRenderer = actionPermission => {
              return (
                <div className={styles.text_center}>
                  {actionPermission &&
                  actionPermission.allow_aggregations === true
                    ? fullAccessDisplay
                    : noAccessDisplay}
                </div>
              );
            };

            return getRolesCells(currTableInfo, roleAggregateAllowedRenderer);
          };

          rowRows.push(
            <tr key={'rows'}>
              <th className={styles.selected + ' ' + styles.text_gray}>Rows</th>
              {getTableActionRolesRowPermissions()}
            </tr>
          );

          if (currAction === 'select') {
            rowRows.push(
              <tr key={'row-limit'}>
                <th className={styles.selected + ' ' + styles.text_gray}>
                  Rows limit
                </th>
                {getTableActionRolesRowLimits()}
              </tr>
            );

            rowRows.push(
              <tr key={'aggregations-allowed'}>
                <th className={styles.selected + ' ' + styles.text_gray}>
                  Aggregations allowed
                </th>
                {getTableActionRolesAggregationAllowed()}
              </tr>
            );
          }

          return rowRows;
        };

        const getColumnRows = () => {
          return currTableInfo.columns.map(column => {
            const columnName = column.column_name;

            const getTableActionRolesColumnPermissions = () => {
              const roleColumnPermissionRenderer = actionPermission => {
                const columnAllowed =
                  actionPermission &&
                  actionPermission.columns &&
                  actionPermission.columns.includes(columnName);

                return columnAllowed ? fullAccessDisplay : noAccessDisplay;
              };

              return getRolesCells(currTableInfo, roleColumnPermissionRenderer);
            };

            return (
              <tr key={columnName}>
                <th className={styles.selected}>{columnName}</th>
                {getTableActionRolesColumnPermissions()}
              </tr>
            );
          });
        };

        return (
          <table
            className={`table table-bordered ${styles.rolesTable} ${styles.remove_margin}`}
          >
            <thead>
              <tr>
                {getActionSelector()}
                <RolesHeader selectable={false} {...defaultRolesHeaderProps} />
              </tr>
            </thead>
            <tbody>
              {getRowRows()}
              {getColumnRows()}
            </tbody>
          </table>
        );
      };

      return (
        <div className={styles.displayFlexContainer}>
          <div className={styles.flex_0}>{getTablesColumnTable()}</div>
          <div>{getTableAllRolesAllActionsTable()}</div>
        </div>
      );
    };

    const getRoleAllTablesTable = () => {
      const getRolesRowTable = () => {
        const getRolesHeaderRow = () => {
          return (
            <tr>
              {getBackBtn('currRole')}
              <RolesHeader
                selectable
                selectedFirst
                {...defaultRolesHeaderProps}
              />
            </tr>
          );
        };

        return (
          <table
            className={`table table-bordered ${styles.rolesTable} ${styles.remove_margin}`}
          >
            <thead>{getRolesHeaderRow()}</thead>
          </table>
        );
      };

      const getRoleAllTablesAllActionsTable = () => {
        const getActionsHeaderRow = () => {
          const getActionsHeaders = () => {
            return allActions.map(action => <th key={action}>{action}</th>);
          };

          return (
            <tr className={styles.selected}>
              <th />
              {getActionsHeaders()}
            </tr>
          );
        };

        const getRoleAllTablesAllActionsRows = () => {
          const tablePermissionListRenderer = table => {
            return getActionsCells(table, rolePermissionsRenderer);
          };

          return getTablesRows(tablePermissionListRenderer, false);
        };

        return (
          <table
            className={`table table-bordered ${styles.rolesTable} ${styles.remove_margin}`}
          >
            <thead>{getActionsHeaderRow()}</thead>
            <tbody>{getRoleAllTablesAllActionsRows()}</tbody>
          </table>
        );
      };

      return (
        <div>
          {getRolesRowTable()}
          {getRoleAllTablesAllActionsTable()}
        </div>
      );
    };

    const getAllTablesAllRolesTable = () => {
      const getHeaderRow = () => {
        return (
          <tr>
            {getActionSelector()}
            <RolesHeader {...defaultRolesHeaderProps} />
          </tr>
        );
      };

      const getAllTableAllRolesRows = () => {
        const tablePermissionListRenderer = table => {
          return getRolesCells(table, rolePermissionsRenderer);
        };

        return getTablesRows(tablePermissionListRenderer);
      };

      return (
        <table className={`table table-bordered ${styles.rolesTable}`}>
          <thead>{getHeaderRow()}</thead>
          <tbody>{getAllTableAllRolesRows()}</tbody>
        </table>
      );
    };

    // ------------------------------------------------------------------------------

    const getTable = () => {
      let tableHtml;

      if (!currRole && !currTable) {
        tableHtml = getAllTablesAllRolesTable();
      } else if (!currRole && currTable) {
        tableHtml = getTableAllRolesTable();
      } else if (currRole && !currTable) {
        tableHtml = getRoleAllTablesTable();
      }

      return tableHtml;
    };

    const getCopyModal = () => {
      const {
        copyFromRole,
        copyFromTable,
        copyFromAction,
        copyToRoles,
        newRole,
      } = copyState;

      const onClose = () => {
        this.setState({ copyState: { ...this.initState.copyState } });
      };

      const onSave = () => {
        if (!copyToRoles.length) {
          document.getElementsByClassName('role-selector')[0].focus();
          return;
        }

        const confirmMessage = 'This will overwrite any existing permissions';
        const isOk = getConfirmation(confirmMessage);
        if (isOk) {
          const onSuccess = () => {
            this.setState({ copyState: { ...this.initState.copyState } });
          };

          dispatch(
            copyRolePermissions(
              copyState.copyFromRole,
              copyState.copyFromTable,
              copyState.copyFromAction,
              copyState.copyToRoles,
              onSuccess
            )
          );
        }
      };

      const getFromRoleOptions = () => {
        return allRoles.map(role => {
          return (
            <option key={role} value={role}>
              {role}
            </option>
          );
        });
      };

      const getFromTableOptions = () => {
        return currSchemaTrackedTables.map(table => {
          const tableName = table.table_name;
          const tableValue = getTableNameWithSchema(getTableDef(table));

          return (
            <option key={tableValue} value={tableValue}>
              {tableName}
            </option>
          );
        });
      };

      const getFromActionOptions = () => {
        return allActions.map(action => {
          return (
            <option key={action} value={action}>
              {action}
            </option>
          );
        });
      };

      const onFromRoleChange = e => {
        this.setState({
          copyState: { ...copyState, copyFromRole: e.target.value },
        });
      };

      const onFromTableChange = e => {
        this.setState({
          copyState: { ...copyState, copyFromTable: e.target.value },
        });
      };

      const onFromActionChange = e => {
        this.setState({
          copyState: { ...copyState, copyFromAction: e.target.value },
        });
      };

      const getToRolesList = () => {
        const _toRolesList = [];

        const getRoleSelector = (value, position) => {
          const getToRoleOptions = () => {
            return allRoles
              .filter(
                role =>
                  role === value ||
                  (role !== copyFromRole && !copyToRoles.includes(role))
              )
              .map(role => {
                return (
                  <option key={role} value={role}>
                    {role}
                  </option>
                );
              });
          };

          const onSelect = e => {
            const _newCopyToRoles = [...copyToRoles];

            const selectedValue = e.target.value;

            if (selectedValue) {
              _newCopyToRoles[position] = selectedValue;
            } else {
              _newCopyToRoles.splice(position, 1);
            }

            this.setState({
              copyState: {
                ...copyState,
                copyToRoles: _newCopyToRoles,
              },
            });
          };

          return (
            <select
              key={value}
              className={
                'role-selector form-control ' + styles.add_mar_top_small
              }
              value={value}
              onChange={onSelect}
            >
              <option value={''}>--</option>
              {getToRoleOptions()}
            </select>
          );
        };

        // add already selected roles
        copyToRoles.forEach((copyToRole, position) => {
          _toRolesList.push(getRoleSelector(copyToRole, position));
        });

        // add empty role selector at end
        _toRolesList.push(getRoleSelector('', copyToRoles.length));

        return _toRolesList;
      };

      const getNewRoleCreator = () => {
        const setNewRole = e => {
          this.setState({
            copyState: {
              ...copyState,
              newRole: e.target.value,
            },
          });
        };

        const addNewRole = () => {
          const _newCopyToRoles = [...copyToRoles];

          if (newRole) {
            _newCopyToRoles.push(newRole);

            this.setState({
              copyState: {
                ...copyState,
                copyToRoles: _newCopyToRoles,
                newRole: '',
              },
            });
          }
        };

        return (
          <div className={styles.display_flex}>
            <input
              type="text"
              className={'form-control'}
              placeholder="new_role"
              value={newRole}
              onChange={setNewRole}
            />
            <Button
              color="white"
              size="xs"
              onClick={addNewRole}
              title="Create new role"
              className={styles.add_mar_left_mid}
            >
              <i className="fa fa-plus" aria-hidden="true" />
            </Button>
          </div>
        );
      };

      return (
        <Modal
          show={copyFromRole !== ''}
          title={'Copy permissions'}
          onClose={onClose}
          onSubmit={onSave}
          submitText={'Copy'}
          submitTestId={'copy-roles-button'}
        >
          <div>
            <div>
              <b>From:</b>
              <div className={styles.add_mar_top_small}>
                <div className="row form-row">
                  <div className="form-group col-md-4">
                    <label>Role</label>
                    <select
                      className={'form-control ' + styles.add_mar_top_small}
                      value={copyFromRole}
                      onChange={onFromRoleChange}
                    >
                      {getFromRoleOptions()}
                    </select>
                  </div>
                  <div className="form-group col-md-4">
                    <label>Table</label>
                    <select
                      className={'form-control ' + styles.add_mar_top_small}
                      value={copyFromTable}
                      onChange={onFromTableChange}
                    >
                      <option key={'all'} value={'all'}>
                        All
                      </option>
                      {getFromTableOptions()}
                    </select>
                  </div>
                  <div className="form-group col-md-4">
                    <label>Action</label>
                    <select
                      className={'form-control ' + styles.add_mar_top_small}
                      value={copyFromAction}
                      onChange={onFromActionChange}
                    >
                      <option key={'all'} value={'all'}>
                        All
                      </option>
                      {getFromActionOptions()}
                    </select>
                  </div>
                </div>
              </div>
            </div>
            <div className={styles.add_mar_top}>
              <b>To:</b>
              <div className={styles.add_mar_top_small}>
                <div className="row form-row">
                  <div className="form-group col-md-4">
                    <label>Roles</label>
                    {getToRolesList()}
                    <div
                      className={
                        styles.add_mar_top +
                        ' ' +
                        styles.add_mar_bottom +
                        ' ' +
                        styles.add_mar_left
                      }
                    >
                      OR
                    </div>
                    {getNewRoleCreator()}
                  </div>
                </div>
              </div>
            </div>
          </div>
        </Modal>
      );
    };

    return (
      <RightContainer>
        <div
          className={`${styles.clear_fix} ${styles.padd_left} ${styles.fit_content}`}
        >
          <Helmet title="Permissions Summary | Hasura" />
          <div className={styles.add_mar_bottom}>
            <h2 className={styles.heading_text}>
              Permissions summary - {currentSchema}
            </h2>
          </div>

          {getTable()}

          {getCopyModal()}
        </div>
      </RightContainer>
    );
  }
}

const permissionsSummaryConnector = connect => {
  const mapStateToProps = state => {
    return {
      allSchemas: state.tables.allSchemas,
      currentSchema: state.tables.currentSchema,
      currentSource: state.tables.currentDataSource,
    };
  };
  return connect(mapStateToProps)(PermissionsSummary);
};

export default permissionsSummaryConnector;
