import React, { Component } from 'react';
import Helmet from 'react-helmet';
import { push } from 'react-router-redux';

import styles from './Roles.scss';

import {
  findTable,
  getTableSchema,
  getTableName,
  checkIfTable,
} from '../../Common/utils/pgSchemaUtils';
import { getTablePermissionsRoute } from '../../Common/utils/routesUtils';

import { updateSchemaInfo } from '../Data/DataActions';
import { permOpenEdit } from '../Data/TablePermissions/Actions';

import {
  permissionsSymbols,
  getAllRoles,
  getPermissionFilterString,
  getPermissionColumnAccessSummary,
  getTablePermissionsByRoles,
  getPermissionRowAccessSummary,
} from './utils';

class Roles extends Component {
  constructor(props) {
    super(props);

    this.state = {
      currRole: null,
      currTable: null,
      currAction: 'select',
    };

    this.props.dispatch(
      updateSchemaInfo({ schemas: [this.props.currentSchema] })
    );
  }

  render() {
    const { currRole, currAction, currTable } = this.state;
    const { dispatch } = this.props;

    // ------------------------------------------------------------------------------

    const { allSchemas } = this.props;

    const allActions = ['select', 'insert', 'update', 'delete'];

    const allRoles = getAllRoles(allSchemas);

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

    const getEditIcon = () => {
      return (
        <span className={styles.editPermsIcon}>
          <i className="fa fa-pencil" aria-hidden="true" />
        </span>
      );
    };

    // ------------------------------------------------------------------------------

    const getRolesHeaders = (selectable = true, selectedFirst = false) => {
      const rolesHeaders = [];

      allRoles.forEach(role => {
        const isCurrRole = currRole === role;

        const setRole = () => {
          this.setState({ currRole: isCurrRole ? null : role });
          window.scrollTo(0, 0);
        };

        const roleHeader = (
          <th
            key={role}
            onClick={selectable ? setRole : null}
            className={`${selectable ? styles.cursorPointer : ''} ${
              isCurrRole ? styles.selected : ''
            }`}
          >
            {role}
          </th>
        );

        if (selectedFirst && isCurrRole) {
          rolesHeaders.unshift(roleHeader);
        } else {
          rolesHeaders.push(roleHeader);
        }
      });

      return rolesHeaders;
    };

    const getRoleCellOnClick = (table, role, action) => {
      return () => {
        dispatch(push(getTablePermissionsRoute(table)));

        if (role && action) {
          // TODO: fix this. above redirect clears state set by this
          dispatch(permOpenEdit(table, role, action));
        }
      };
    };

    const getRolesCells = (table, roleCellRenderer) => {
      const tablePermissions = getTablePermissionsByRoles(table);

      return allRoles.map(role => {
        const rolePermissions = tablePermissions[role];
        const actionPermission = rolePermissions
          ? rolePermissions[currAction]
          : null;

        return (
          <td
            key={role}
            className={styles.clickableCell}
            onClick={getRoleCellOnClick(table, role, currAction)}
          >
            <div
              className={styles.display_flex + ' ' + styles.flex_space_between}
            >
              <div>{roleCellRenderer(actionPermission, table)}</div>
              <div>{getEditIcon()}</div>
            </div>
          </td>
        );
      });
    };

    const getActionsCells = (table, actionCellRenderer) => {
      const tablePermissions = getTablePermissionsByRoles(table);
      const rolePermission = tablePermissions[currRole] || {};

      return allActions.map(action => {
        const actionPermission = rolePermission[action];
        return (
          <td key={action}>
            {actionCellRenderer(actionPermission, table, action)}
          </td>
        );
      });
    };

    const getTablesRows = (
      tableRowCellRenderer,
      selectable = true,
      selectedFirst = false
    ) => {
      const tablesRows = [];

      allSchemas.forEach((table, i) => {
        const tableName = getTableName(table);
        const tableSchema = getTableSchema(table);
        const isTable = checkIfTable(table);

        const isCurrTable =
          currTable &&
          currTable.tableName === tableName &&
          currTable.tableSchema === tableSchema;

        const getTableHead = () => {
          const setTable = () => {
            this.setState({
              currTable: isCurrTable ? null : { tableName, tableSchema },
            });
          };

          return (
            <th
              key={tableName}
              onClick={selectable ? setTable : null}
              className={`${selectable ? styles.cursorPointer : ''} ${
                isCurrTable ? styles.selected : ''
              }`}
            >
              {isTable ? <span>{tableName}</span> : <i>{tableName}</i>}
            </th>
          );
        };

        const tableRow = (
          <tr key={i}>
            {getTableHead()}
            {tableRowCellRenderer(table)}
          </tr>
        );

        if (selectedFirst && isCurrTable) {
          tablesRows.unshift(tableRow);
        } else {
          tablesRows.push(tableRow);
        }
      });

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
                  {getPermissionColumnAccessSummary(
                    actionPermission,
                    table.columns
                  )}
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
      const currTableInfo = findTable(
        allSchemas,
        currTable.tableName,
        currTable.tableSchema
      );
      const getTablesColumnTable = () => {
        return (
          <table
            className={`table table-bordered ${styles.rolesTable} ${
              styles.remove_margin
            }`}
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
            className={`table table-bordered ${styles.rolesTable} ${
              styles.remove_margin
            }`}
          >
            <thead>
              <tr>
                <th className={styles.selected}>{getActionSelector()}</th>
                {getRolesHeaders(false)}
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
              {getRolesHeaders(true, true)}
            </tr>
          );
        };

        return (
          <table
            className={`table table-bordered ${styles.rolesTable} ${
              styles.remove_margin
            }`}
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
            className={`table table-bordered ${styles.rolesTable} ${
              styles.fixed
            } ${styles.remove_margin}`}
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
            {getRolesHeaders()}
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

    return (
      <div className={`container-fluid ${styles.full_container}`}>
        <div className={styles.subHeader}>
          <Helmet title={'Roles | Hasura'} />
          <h2 className={styles.headerText}>Roles</h2>
        </div>

        <div className={styles.add_mar_top}>{getTable()}</div>
      </div>
    );
  }
}

const rolesConnector = connect => {
  const mapStateToProps = state => {
    return {
      allSchemas: state.tables.allSchemas,
      currentSchema: state.tables.currentSchema,
    };
  };
  return connect(mapStateToProps)(Roles);
};

export default rolesConnector;
