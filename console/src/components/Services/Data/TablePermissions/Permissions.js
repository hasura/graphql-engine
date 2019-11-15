import PropTypes from 'prop-types';
import React, { Component } from 'react';
import AceEditor from 'react-ace';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import InputGroup from 'react-bootstrap/lib/InputGroup';
import OverlayTrigger from 'react-bootstrap/es/OverlayTrigger';
import 'brace/mode/json';
import 'brace/theme/github';

import { RESET } from '../TableModify/ModifyActions';
import {
  permChangeTypes,
  permOpenEdit,
  permSetFilter,
  permSetFilterSameAs,
  permToggleColumn,
  permToggleAllColumns,
  permAllowAll,
  permCloseEdit,
  permSetRoleName,
  permChangePermissions,
  // permToggleAllowUpsert,
  permToggleAllowAggregation,
  permToggleModifyLimit,
  permCustomChecked,
  // permRemoveRole,
  permSetBulkSelect,
  permRemoveMultipleRoles,
  permSetApplySamePerm,
  permDelApplySamePerm,
  applySamePermissionsBulk,
  SET_PRESET_VALUE,
  CREATE_NEW_PRESET,
  DELETE_PRESET,
  X_HASURA_CONST,
} from './Actions';

import PermissionBuilder from './PermissionBuilder/PermissionBuilder';
import TableHeader from '../TableCommon/TableHeader';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';
import EnhancedInput from '../../../Common/InputChecker/InputChecker';

import { setTable, updateSchemaInfo } from '../DataActions';
import { getIngForm, getEdForm, escapeRegExp } from '../utils';
import { allOperators, getLegacyOperator } from './PermissionBuilder/utils';
import {
  permissionsSymbols,
  getAllRoles,
  getPermissionFilterString,
  getPermissionColumnAccessSummary,
  getTablePermissionsByRoles,
  getPermissionRowAccessSummary,
} from '../PermissionsSummary/utils';
import Button from '../../../Common/Button/Button';
import { defaultPresetsState } from '../DataState';

import { NotFoundError } from '../../../Error/PageNotFound';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { generateTableDef } from '../../../Common/utils/pgUtils';

class Permissions extends Component {
  constructor() {
    super();

    this.state = {
      presetsInfo: {
        insert: {
          columnTypeMap: {},
        },
        update: {
          columnTypeMap: {},
        },
      },
    };
  }

  componentDidMount() {
    this.props.dispatch({ type: RESET });
    this.props.dispatch(setTable(this.props.tableName));
  }

  componentDidUpdate(prevProps) {
    const currPermissionsState = this.props.permissionsState;
    const prevPermissionsState = prevProps.permissionsState;

    // scroll to edit section if role/query change
    if (
      (currPermissionsState.role &&
        currPermissionsState.role !== prevPermissionsState.role) ||
      (currPermissionsState.query &&
        currPermissionsState.query !== prevPermissionsState.query)
    ) {
      document
        .getElementById('permission-edit-section')
        .scrollIntoView({ behavior: 'smooth' });
    }

    if (
      !prevPermissionsState.bulkSelect.length &&
      currPermissionsState.bulkSelect.length
    ) {
      document
        .getElementById('bulk-section')
        .scrollIntoView({ behavior: 'smooth' });
    }
  }

  render() {
    const {
      dispatch,
      tableName,
      tableType,
      allSchemas,
      schemaList,
      ongoingRequest,
      lastError,
      lastFormError,
      lastSuccess,
      permissionsState,
      migrationMode,
      currentSchema,
    } = this.props;

    const currentTableSchema = this.props.allSchemas.find(
      t =>
        t.table_name === this.props.tableName &&
        t.table_schema === this.props.currentSchema
    );

    if (!currentTableSchema) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const styles = require('./Permissions.scss');

    const addTooltip = (text, tooltip) => {
      return (
        <span>
          <span className={styles.add_mar_right_small}>{text}</span>
          <OverlayTrigger placement="right" overlay={tooltip}>
            <i className="fa fa-question-circle" aria-hidden="true" />
          </OverlayTrigger>
        </span>
      );
    };

    /********************/

    const getAlertHtml = (
      _ongoingRequest,
      _lastError,
      _lastSuccess,
      _lastFormError
    ) => {
      let alertText = '';
      let alertStyle = '';
      if (_ongoingRequest) {
        alertText = 'Saving...';
        alertStyle = 'alert-warning';
      } else if (_lastError) {
        alertText = `Error: ${JSON.stringify(_lastError)}`;
        alertStyle = 'alert-danger';
      } else if (_lastSuccess) {
        alertText = 'Saved!';
        alertStyle = 'alert-success';
      } else if (_lastFormError) {
        alertText = _lastFormError;
        alertStyle = 'alert-warning';
      }

      return (
        <div className={`hidden alert ${alertStyle}`} role="alert">
          {alertText}
        </div>
      );
    };

    const getHeader = tableSchema => {
      return (
        <TableHeader
          dispatch={dispatch}
          table={tableSchema}
          tabName="permissions"
          migrationMode={migrationMode}
        />
      );
    };

    const getPermissionsTable = (tableSchema, queryTypes, roleList) => {
      const getPermissionsLegend = () => (
        <div>
          <div className={styles.permissionsLegend}>
            <span className={styles.permissionsLegendValue}>
              {permissionsSymbols.fullAccess} : full access
            </span>
            <span className={styles.permissionsLegendValue}>
              {permissionsSymbols.noAccess} : no access
            </span>
            <span className={styles.permissionsLegendValue}>
              {permissionsSymbols.partialAccess} : partial access
            </span>
          </div>
        </div>
      );

      const getViewPermissionNote = () => {
        let note;

        let hasPermissions = true;
        if (
          tableType === 'view' &&
          !(
            tableSchema.view_info &&
            (tableSchema.view_info.is_insertable_into === 'YES' ||
              tableSchema.view_info.is_trigger_insertable_into === 'YES') &&
            (tableSchema.view_info.is_updatable === 'YES' ||
              tableSchema.view_info.is_trigger_updatable === 'YES')
          )
        ) {
          hasPermissions = false;
        }

        if (!hasPermissions) {
          note = (
            <div className={styles.permissionsLegend}>
              <i className="fa fa-info-circle" aria-hidden="true" />
              &nbsp; You cannot insert/update into this view
            </div>
          );
        }

        return note;
      };

      const getPermissionsTableHead = () => {
        const _permissionsHead = [];

        // push role head
        _permissionsHead.push(<th key={-2}>Role</th>);

        // push action heads
        queryTypes.forEach((queryType, i) => {
          _permissionsHead.push(<th key={i}>{queryType}</th>);
        });

        // push bulk actions head
        _permissionsHead.push(<th key={-1} />);

        return (
          <thead>
            <tr>{_permissionsHead}</tr>
          </thead>
        );
      };

      const getPermissionsTableBody = () => {
        const _permissionsRowsHtml = [];

        const getPermissionsTableRow = (role, newPermRow = null) => {
          const dispatchOpenEdit = queryType => () => {
            if (role === '') {
              document.getElementById('new-role-input').focus();
            } else {
              dispatch(permOpenEdit(tableSchema, role, queryType));
            }
          };

          const dispatchCloseEdit = () => {
            dispatch(permCloseEdit());
          };

          const dispatchBulkSelect = e => {
            const isChecked = e.target.checked;
            const selectedRole = e.target.getAttribute('data-role');
            dispatch(permSetBulkSelect(isChecked, selectedRole));
          };

          // const dispatchDeletePermission = () => {
          //   const confirmMessage = `This will delete the currently set permissions for role "${role}"`;
          //   const isOk = getConfirmation(confirmMessage);
          //   if (isOk) {
          //     dispatch(permRemoveRole(tableSchema, role));
          //   }
          // };

          const getEditIcon = () => {
            return (
              <span className={styles.editPermsIcon}>
                <i className="fa fa-pencil" aria-hidden="true" />
              </span>
            );
          };

          const getRoleQueryPermission = queryType => {
            let _permission;

            const rolePermissions = getTablePermissionsByRoles(tableSchema);

            if (role === 'admin') {
              _permission = permissionsSymbols.fullAccess;
            } else if (!Object.keys(rolePermissions).includes(role)) {
              _permission = permissionsSymbols.noAccess;
            } else {
              const permissions = rolePermissions[role][queryType];

              if (permissions) {
                let checkColumns;
                let filterKey;

                if (queryType === 'select' || queryType === 'update') {
                  checkColumns = true;
                  filterKey = 'filter';
                } else if (queryType === 'insert') {
                  checkColumns = true;
                  filterKey = 'check';
                } else if (queryType === 'delete') {
                  checkColumns = false;
                  filterKey = 'filter';
                }

                if (JSON.stringify(permissions[filterKey]) === '{}') {
                  if (
                    checkColumns &&
                    (!permissions.columns ||
                      (!permissions.columns.includes('*') &&
                        permissions.columns.length !==
                          tableSchema.columns.length))
                  ) {
                    _permission = permissionsSymbols.partialAccess;
                  } else {
                    _permission = permissionsSymbols.fullAccess;
                  }
                } else {
                  _permission = permissionsSymbols.partialAccess;
                }
              } else {
                _permission = permissionsSymbols.noAccess;
              }
            }

            return _permission;
          };

          const _permissionsRowHtml = [];

          // push role value
          if (!newPermRow) {
            _permissionsRowHtml.push(<th key={-2}>{role}</th>);
          } else {
            const dispatchRoleNameChange = e => {
              dispatch(permSetRoleName(e.target.value));
            };

            _permissionsRowHtml.push(
              <th key={-2}>
                <input
                  id="new-role-input"
                  className={`form-control ${styles.newRoleInput}`}
                  onChange={dispatchRoleNameChange}
                  type="text"
                  placeholder="Enter new role"
                  value={role}
                  data-test="role-textbox"
                />
              </th>
            );
          }

          // push action permission value
          queryTypes.forEach((queryType, i) => {
            const isEditAllowed = role !== 'admin';
            const isCurrEdit =
              permissionsState.role === role &&
              permissionsState.query === queryType;

            let editIcon = '';
            let className = '';
            let onClick = () => {};
            if (isEditAllowed) {
              className += styles.clickableCell;
              editIcon = getEditIcon();

              if (isCurrEdit) {
                onClick = dispatchCloseEdit;
                className += ' ' + styles.currEdit;
              } else {
                onClick = dispatchOpenEdit(queryType);
              }
            }

            _permissionsRowHtml.push(
              <td
                key={i}
                className={className}
                onClick={onClick}
                title="Edit permissions"
                data-test={`${role}-${queryType}`}
              >
                {getRoleQueryPermission(queryType)}
                {editIcon}
              </td>
            );
          });

          // push bulk action value
          if (role === 'admin' || role === '') {
            _permissionsRowHtml.push(<td key={-1} />);
          } else {
            const bulkSelect = permissionsState.bulkSelect;

            // const deleteIcon = (
            //   <i
            //     onClick={dispatchDeletePermission}
            //     className={styles.permissionDelete + ' fa fa-close'}
            //     title="Remove all permissions"
            //     aria-hidden="true"
            //   />
            // );

            _permissionsRowHtml.push(
              <td key={-1}>
                <div>
                  <input
                    onChange={dispatchBulkSelect}
                    checked={bulkSelect.filter(e => e === role).length}
                    data-role={role}
                    title="Select for bulk actions"
                    type="checkbox"
                  />
                  {/*{deleteIcon}*/}
                </div>
              </td>
            );
          }

          return _permissionsRowHtml;
        };

        // add admin to roles
        const _roleList = ['admin'].concat(roleList);

        // add existing roles rows
        _roleList.forEach((role, i) => {
          _permissionsRowsHtml.push(
            <tr key={i}>{getPermissionsTableRow(role)}</tr>
          );
        });

        // add new role row
        _permissionsRowsHtml.push(
          <tr key="newPerm">
            {getPermissionsTableRow(permissionsState.newRole, true)}
          </tr>
        );

        return <tbody>{_permissionsRowsHtml}</tbody>;
      };

      return (
        <div>
          {getPermissionsLegend()}
          {getViewPermissionNote()}
          <table className={`table table-bordered ${styles.permissionsTable}`}>
            {getPermissionsTableHead()}
            {getPermissionsTableBody()}
          </table>
        </div>
      );
    };

    const getBulkSection = tableSchema => {
      const bulkSelectedRoles = permissionsState.bulkSelect;

      if (!bulkSelectedRoles.length) {
        return;
      }

      const getSelectedRoles = () => {
        return bulkSelectedRoles.map(r => {
          return (
            <span key={r} className={styles.add_pad_right}>
              <b>{r}</b>{' '}
            </span>
          );
        });
      };

      const handleBulkRemoveClick = () => {
        const confirmMessage =
          'This will remove all currently set permissions for the selected role(s)';
        const isOk = getConfirmation(confirmMessage);
        if (isOk) {
          dispatch(permRemoveMultipleRoles(tableSchema));
        }
      };

      return (
        <div id={'bulk-section'} className={styles.activeEdit}>
          <div className={styles.editPermsHeading}>Apply Bulk Actions</div>
          <div>
            <span className={styles.add_pad_right}>Selected Roles</span>
            {getSelectedRoles()}
          </div>
          <div className={styles.add_mar_top + ' ' + styles.add_mar_bottom_mid}>
            <Button onClick={handleBulkRemoveClick} color="red" size="sm">
              Remove All Permissions
            </Button>
          </div>
        </div>
      );
    };

    const getEditSection = (tableSchema, queryTypes, roleList) => {
      if (!permissionsState.isEditing) {
        return;
      }

      const dispatchCloseEdit = () => {
        dispatch(permCloseEdit());
      };

      const query = permissionsState.query;

      const noPermissions = !permissionsState[query];
      const noPermissionsMsg = 'Set row permissions first';

      let sectionClasses = styles.editPermsSection;
      if (noPermissions) {
        sectionClasses += ' ' + styles.disabled;
      }

      const getSectionHeader = (title, toolTip, sectionStatus) => {
        let sectionStatusHtml;
        if (sectionStatus) {
          sectionStatusHtml = (
            <span className={styles.add_mar_left}>
              - <i className={styles.sectionStatus}>{sectionStatus}</i>
            </span>
          );
        }

        return (
          <div>
            {addTooltip(title, toolTip)} {sectionStatusHtml}
          </div>
        );
      };

      const getRowSection = () => {
        let filterString = getPermissionFilterString(
          permissionsState[query],
          query
        );

        const rowSectionStatus = getPermissionRowAccessSummary(filterString);

        // replace legacy operator values
        allOperators.forEach(operator => {
          const currentString = '"' + operator + '"';
          const legacyString = '"' + getLegacyOperator(operator) + '"';

          filterString = filterString.replace(
            new RegExp(escapeRegExp(legacyString), 'g'),
            currentString
          );
        });

        const getFilterOptions = () => {
          const dispatchAllowAll = () => {
            dispatch(permAllowAll());
          };

          const dispatchSetFilterSameAs = filter => () => {
            dispatch(permSetFilterSameAs(JSON.parse(filter)));
          };

          const dispatchCustomChecked = () => {
            dispatch(permCustomChecked());
          };

          // return queries grouped by filterString i.e. { filterString: [query] }
          const getFilterQueries = () => {
            const _filterQueries = {};
            queryTypes.forEach(queryType => {
              if (queryType === permissionsState.query) {
                return;
              }

              let queryFilterString = '';
              if (permissionsState[queryType]) {
                queryFilterString = getPermissionFilterString(
                  permissionsState[queryType],
                  queryType
                );
              }

              if (queryFilterString) {
                _filterQueries[queryFilterString] =
                  _filterQueries[queryFilterString] || [];
                _filterQueries[queryFilterString].push(queryType);
              }
            });

            return _filterQueries;
          };

          const _filterOptionsSection = [];

          const filterQueries = getFilterQueries();

          const selectedValue = (
            <AceEditor
              mode="json"
              value={filterString}
              readOnly
              theme="github"
              height="5em"
              maxLines={5}
              width="100%"
              showPrintMargin={false}
              key={-3}
            />
          );

          const getFilterRadio = (key, checked, value, onClick, label) => (
            <div className="radio" key={key}>
              <label>
                <input
                  type="radio"
                  checked={checked}
                  value={value}
                  onClick={onClick}
                  readOnly
                />
                {label}
              </label>
            </div>
          );

          // TODO: add no access option

          const addNoChecksOption = () => {
            const isSelected =
              !permissionsState.custom_checked &&
              rowSectionStatus === 'without any checks';

            // Add allow all option
            let allowAllQueryInfo = '';
            if (filterQueries['{}']) {
              allowAllQueryInfo = (
                <i className={styles.add_mar_left_small}>
                  (Same as <b>{filterQueries['{}'].join(', ')}</b>)
                </i>
              );
            }

            const allowAllLabel = (
              <span data-test="without-checks">
                Without any checks {allowAllQueryInfo}
              </span>
            );

            _filterOptionsSection.push(
              getFilterRadio(
                -1,
                isSelected,
                'AllowAll',
                dispatchAllowAll,
                allowAllLabel
              )
            );

            if (isSelected) {
              _filterOptionsSection.push(selectedValue);
            }
          };

          const addSameAsOptions = () => {
            // Add other query options
            Object.keys(filterQueries).forEach((filter, i) => {
              if (filter === '{}') {
                return;
              }

              const isSelected =
                !permissionsState.custom_checked && filterString === filter;

              const queries = filterQueries[filter].join(', ');
              const queryLabel = (
                <span data-test="mutual-check">
                  With same custom check as <b>{queries}</b>
                </span>
              );
              _filterOptionsSection.push(
                getFilterRadio(
                  i,
                  isSelected,
                  queries,
                  dispatchSetFilterSameAs(filter),
                  queryLabel
                )
              );

              if (isSelected) {
                _filterOptionsSection.push(selectedValue);
              }
            });
          };

          const addCustomCheckOption = () => {
            const dispatchFuncSetFilter = filter =>
              permSetFilter(JSON.parse(filter));

            const loadSchemasFunc = schemaNames => {
              dispatch(updateSchemaInfo({ schemas: schemaNames }));
            };

            const isUniqueFilter =
              filterString !== '' &&
              filterString !== '{}' &&
              !filterQueries[filterString];

            const isSelected =
              permissionsState.custom_checked || isUniqueFilter;

            const customCheckToolTip = (
              <Tooltip id="tooltip-custom-check">
                Create custom check using permissions builder
              </Tooltip>
            );

            const customChecklabel = (
              <span data-test="custom-check">
                <span className={styles.add_mar_right}>With custom check:</span>
                <OverlayTrigger placement="right" overlay={customCheckToolTip}>
                  <i className="fa fa-question-circle" aria-hidden="true" />
                </OverlayTrigger>
              </span>
            );

            _filterOptionsSection.push(
              getFilterRadio(
                -2,
                isSelected,
                'Custom',
                dispatchCustomChecked,
                customChecklabel
              )
            );

            if (isSelected) {
              _filterOptionsSection.push(selectedValue);

              _filterOptionsSection.push(
                <PermissionBuilder
                  dispatchFuncSetFilter={dispatchFuncSetFilter}
                  loadSchemasFunc={loadSchemasFunc}
                  tableDef={generateTableDef(tableName, currentSchema)}
                  allTableSchemas={allSchemas}
                  schemaList={schemaList}
                  filter={filterString}
                  dispatch={dispatch}
                  key={-4}
                />
              );
            }
          };

          addNoChecksOption();
          addSameAsOptions();
          addCustomCheckOption();

          return _filterOptionsSection;
        };

        const getLimitSection = () => {
          const dispatchLimit = limit => {
            const parsedLimit = parseInt(limit, 10);
            dispatch(permToggleModifyLimit(parsedLimit));
          };

          let _limitSection;

          const rowLimitTooltip = (
            <Tooltip id="tooltip-row-permissions">
              Set limit on number of rows fetched per request
            </Tooltip>
          );

          if (query === 'select') {
            const limitValue =
              permissionsState.select && permissionsState.select.limit
                ? permissionsState.select.limit
                : '';

            _limitSection = (
              <div className={styles.inline_block}>
                <label>Limit number of rows:</label>
                <input
                  className={
                    styles.mar_small_left + ' form-control ' + styles.limitInput
                  }
                  value={limitValue}
                  onChange={e => dispatchLimit(e.target.value)}
                  disabled={noPermissions}
                  title={noPermissions ? noPermissionsMsg : ''}
                  type="number"
                  min="0"
                />
                <div className={styles.clear_fix} />
              </div>
            );

            return addTooltip(_limitSection, rowLimitTooltip);
          }
        };

        const rowPermissionTooltip = (
          <Tooltip id="tooltip-row-permissions">
            Set permission rule for {getIngForm(permissionsState.query)} rows
          </Tooltip>
        );

        const rowSectionTitle = 'Row ' + query + ' permissions';

        return (
          <CollapsibleToggle
            title={getSectionHeader(
              rowSectionTitle,
              rowPermissionTooltip,
              rowSectionStatus
            )}
            useDefaultTitleStyle
            testId={'toggle-row-permission'}
            isOpen={rowSectionStatus === 'no access'}
          >
            <div className={styles.editPermsSection}>
              <div>
                <div>
                  Allow role <b>{permissionsState.role}</b> to{' '}
                  {permissionsState.query} <b>rows</b>:
                </div>
                {getFilterOptions()}
              </div>
              <div className={styles.add_mar_top}>{getLimitSection()}</div>
            </div>
          </CollapsibleToggle>
        );
      };

      const getColumnSection = () => {
        const getColumnList = () => {
          const _columnList = [];

          const dispatchToggleColumn = e => {
            const column = e.target.value;
            dispatch(permToggleColumn(column));
          };

          tableSchema.columns.forEach((colObj, i) => {
            const column = colObj.column_name;

            let checked;
            if (permissionsState[query]) {
              if (permissionsState[query].columns === '*') {
                checked = true;
              } else {
                checked = permissionsState[query].columns.includes(column);
              }
            } else {
              checked = false;
            }

            _columnList.push(
              <div key={i} className={styles.columnListElement}>
                <div className="checkbox">
                  <label>
                    <input
                      type="checkbox"
                      checked={checked}
                      value={column}
                      onChange={dispatchToggleColumn}
                      disabled={noPermissions}
                      title={noPermissions ? noPermissionsMsg : ''}
                    />
                    {column}
                  </label>
                </div>
              </div>
            );
          });

          _columnList.push(<div key={-1} className={styles.clear_fix} />);

          return _columnList;
        };

        const getRelationshipsMsg = () => {
          let _relationshipsMsg = '';

          const relationships = tableSchema.relationships.map(
            relObj => relObj.rel_name
          );

          if (relationships.length) {
            _relationshipsMsg = (
              <div className={styles.add_mar_top_small}>
                For <b>relationships</b>, set permissions for the corresponding
                tables/views.
              </div>
            );
          }

          return _relationshipsMsg;
        };

        const getToggleAllBtn = () => {
          const dispatchToggleAllColumns = () => {
            const allColumns = tableSchema.columns.map(c => c.column_name);

            dispatch(permToggleAllColumns(allColumns));
          };

          return (
            <Button
              size={'xs'}
              onClick={dispatchToggleAllColumns}
              disabled={noPermissions}
              title={noPermissions ? noPermissionsMsg : ''}
              data-test={'toggle-all-col-btn'}
            >
              Toggle All
            </Button>
          );
        };

        let _columnSection = '';

        const queriesWithPermColumns = ['select', 'update', 'insert'];

        if (queriesWithPermColumns.includes(query)) {
          const getAccessText = () => {
            let accessText;
            if (query === 'insert') {
              accessText = 'to set input for';
            } else if (query === 'select') {
              accessText = 'to access';
            } else {
              accessText = 'to update';
            }
            return accessText;
          };

          const colPermissionTooltip = (
            <Tooltip id="tooltip-row-permissions">
              Choose columns allowed to be {getEdForm(permissionsState.query)}
            </Tooltip>
          );

          const colSectionTitle = 'Column ' + query + ' permissions';

          const colSectionStatus = getPermissionColumnAccessSummary(
            permissionsState[query],
            tableSchema.columns
          );

          _columnSection = (
            <CollapsibleToggle
              title={getSectionHeader(
                colSectionTitle,
                colPermissionTooltip,
                colSectionStatus
              )}
              useDefaultTitleStyle
              testId={'toggle-col-permission'}
              isOpen={colSectionStatus === 'no columns'}
            >
              <div
                className={sectionClasses}
                title={noPermissions ? noPermissionsMsg : ''}
              >
                <div>
                  <span className={styles.add_mar_right}>
                    Allow role <b>{permissionsState.role}</b> {getAccessText()}{' '}
                    <b>columns</b>:
                  </span>

                  {getToggleAllBtn()}
                </div>

                {getColumnList()}

                {getRelationshipsMsg()}
              </div>
            </CollapsibleToggle>
          );
        }

        return _columnSection;
      };

      // const getUpsertSection = () => {
      //   if (query !== 'insert') {
      //     return;
      //   }
      //
      //   const dispatchToggleAllowUpsert = checked => {
      //     dispatch(permToggleAllowUpsert(checked));
      //   };
      //
      //   const upsertAllowed = permissionsState.insert
      //     ? permissionsState.insert.allow_upsert
      //     : false;
      //
      //   const upsertToolTip = (
      //     <Tooltip id="tooltip-upsert">
      //       Allow upsert queries. Upsert lets you update a row if it already
      //       exists, otherwise insert it
      //     </Tooltip>
      //   );
      //
      //   const upsertStatus = upsertAllowed ? 'enabled' : 'disabled';
      //
      //   return (
      //     <CollapsibleToggle
      //       title={getSectionHeader(
      //         'Upsert queries permissions',
      //         upsertToolTip,
      //         upsertStatus
      //       )}
      //       useDefaultTitleStyle
      //       testId={'toggle-upsert-permission'}
      //     >
      //       <div
      //         className={sectionClasses}
      //         title={noPermissions ? noPermissionsMsg : ''}
      //       >
      //         <div className="checkbox">
      //           <label>
      //             <input
      //               type="checkbox"
      //               checked={upsertAllowed}
      //               value="toggle_upsert"
      //               onChange={e => dispatchToggleAllowUpsert(e.target.checked)}
      //               disabled={noPermissions}
      //             />
      //             Allow role <b>{permissionsState.role}</b> to make upsert
      //             queries
      //           </label>
      //         </div>
      //       </div>
      //     </CollapsibleToggle>
      //   );
      // };

      const getPresetsSection = action => {
        if (query !== action) {
          return;
        }

        const { columns } = tableSchema;
        const queryState = permissionsState[query];

        const presets = (queryState && queryState.localPresets) || [
          defaultPresetsState[query],
        ];

        const getPresetValues = () => {
          const setPresetValue = e => {
            // Get the index of the changed value and if both key and value are set create one more object in set
            const inputNode = e.target;

            const indexId =
              inputNode &&
              parseInt(inputNode.getAttribute('data-index-id'), 10);
            const prefixVal =
              inputNode && inputNode.getAttribute('data-prefix-val');
            const actionData = {};

            if (indexId >= 0) {
              actionData.key = 'value';
              actionData.value = (prefixVal || '') + inputNode.value;
              actionData.index = indexId;

              this.props.dispatch({
                type: SET_PRESET_VALUE,
                data: { ...actionData, queryType: query },
              });
            }
          };

          const setPresetKey = e => {
            // Get the index of the changed value and if both key and value are set create one more object in set
            const selectNode = e.target;
            const selectedOption = e.target.selectedOptions[0];

            const indexId =
              selectNode &&
              parseInt(selectNode.getAttribute('data-index-id'), 10);
            const actionData = {};

            if (selectedOption && indexId >= 0) {
              actionData.key = 'key';
              actionData.value = selectNode.value;
              actionData.index = indexId;

              const columnType = selectedOption.getAttribute(
                'data-column-type'
              );

              this.props.dispatch({
                type: SET_PRESET_VALUE,
                data: { ...actionData, queryType: query },
              });

              if (indexId === presets.length - 1) {
                this.props.dispatch({
                  type: CREATE_NEW_PRESET,
                  data: { query },
                });
              }

              this.setState({
                presetsInfo: {
                  ...this.state.presetsInfo,
                  [query]: {
                    ...this.state.presetsInfo[query],
                    columnTypeMap: {
                      ...this.state.presetsInfo[query].columnTypeMap,
                      [indexId]: columnType,
                    },
                  },
                },
              });
            }
          };

          const setPresetType = e => {
            const selectNode = e.target;

            const indexId =
              selectNode &&
              parseInt(selectNode.getAttribute('data-index-id'), 10);
            if (indexId >= 0) {
              // Clearing the stuff just to filter out errored cases
              const actionData = {};
              actionData.key = 'value';
              actionData.value =
                e.target.value === 'session' ? X_HASURA_CONST : '';
              actionData.index = indexId;

              this.props.dispatch({
                type: SET_PRESET_VALUE,
                data: { ...actionData, queryType: query },
              });
            }
          };

          const deletePreset = e => {
            const deleteIndex = parseInt(
              e.target.getAttribute('data-index-id'),
              10
            );
            if (deleteIndex >= 0) {
              this.props.dispatch({
                type: DELETE_PRESET,
                data: {
                  index: deleteIndex,
                  queryType: query,
                },
              });
            }
          };

          const getPresetValueType = preset => {
            let _valueType = '';

            if (preset.key || preset.value) {
              const value = preset.value;
              if (
                typeof value === 'string' &&
                value.toLowerCase().indexOf(X_HASURA_CONST) === 0
              ) {
                _valueType = 'session';
              } else {
                _valueType = 'static';
              }
            }

            return _valueType;
          };

          const getPresetColumnSelect = (preset, index) => {
            const getColumnOptions = () => {
              const _columnOptions = [];

              _columnOptions.push(
                <option value="" disabled key={-1}>
                  Column Name
                </option>
              );

              if (columns && columns.length > 0) {
                columns.forEach((c, i) => {
                  _columnOptions.push(
                    <option
                      value={c.column_name}
                      data-column-type={c.data_type}
                      key={i}
                    >
                      {c.column_name}
                    </option>
                  );
                });
              }

              return _columnOptions;
            };

            return (
              <select
                className="input-sm form-control"
                value={preset.key}
                onChange={setPresetKey}
                data-index-id={index}
                data-test={'column-presets-column-' + index}
                disabled={noPermissions}
                title={noPermissions ? noPermissionsMsg : ''}
              >
                {getColumnOptions()}
              </select>
            );
          };

          const getPresetTypeSelect = (preset, index) => {
            const presetType = getPresetValueType(preset);

            const selectTypeDisabled = !preset.key;

            return (
              <select
                className="input-sm form-control"
                onChange={setPresetType}
                data-index-id={index}
                data-test={'column-presets-type-' + index}
                value={presetType}
                disabled={selectTypeDisabled}
                title={selectTypeDisabled ? 'Choose column first' : ''}
              >
                <option value="" disabled>
                  Select Preset Type
                </option>
                <option value="static">static</option>
                <option value="session">from session variable</option>
              </select>
            );
          };

          const getPresetInput = (preset, index) => {
            let _presetInput;

            const presetType = getPresetValueType(preset);

            const presetInputDisabled = !preset.key;

            if (presetType === 'session') {
              _presetInput = (
                <InputGroup>
                  <InputGroup.Addon>X-Hasura-</InputGroup.Addon>
                  <input
                    className={'input-sm form-control '}
                    placeholder="column_value"
                    value={preset.value.slice(X_HASURA_CONST.length)}
                    onChange={setPresetValue}
                    data-test={'column-presets-value-' + index}
                    data-index-id={index}
                    data-prefix-val={X_HASURA_CONST}
                  />
                </InputGroup>
              );
            } else {
              _presetInput = (
                <EnhancedInput
                  placeholder="column_value"
                  type={
                    index in this.state.presetsInfo[query].columnTypeMap
                      ? this.state.presetsInfo[query].columnTypeMap[index]
                      : ''
                  }
                  value={preset.value}
                  onChange={setPresetValue}
                  data-test={'column-presets-value-' + index}
                  indexId={index}
                  data-prefix-val={X_HASURA_CONST}
                  disabled={presetInputDisabled}
                  title={presetInputDisabled ? 'Choose column first' : ''}
                />
              );
            }

            return _presetInput;
          };

          const getPresetExample = preset => {
            let _presetExample;

            const presetType = getPresetValueType(preset);

            if (presetType === 'session') {
              _presetExample = 'e.g. X-Hasura-User-Id';
            } else {
              _presetExample = 'e.g. false, 1, some-text';
            }

            return <i>{_presetExample}</i>;
          };

          const getDeleteButton = (preset, index) => {
            let _deleteBtn;

            const presetType = getPresetValueType(preset);

            if (presetType) {
              _deleteBtn = (
                <i
                  className="fa-lg fa fa-times"
                  onClick={deletePreset}
                  data-index-id={index}
                />
              );
            }

            return _deleteBtn;
          };

          return presets.map((preset, i) => {
            const rowElementStyle =
              styles.display_inline +
              ' ' +
              styles.add_mar_right +
              ' ' +
              styles.input_element_wrapper;

            return (
              <div className={styles.insertSetConfigRow} key={i}>
                <div className={rowElementStyle}>
                  {getPresetColumnSelect(preset, i)}
                </div>
                <div className={rowElementStyle}>
                  {getPresetTypeSelect(preset, i)}
                </div>
                <div className={rowElementStyle}>
                  {getPresetInput(preset, i)}
                </div>
                <div className={rowElementStyle}>
                  {getPresetExample(preset)}
                </div>
                <div className={rowElementStyle}>
                  {getDeleteButton(preset, i)}
                </div>
              </div>
            );
          });
        };

        const presetTooltip = (
          <Tooltip id="tooltip-insert-set-operations">
            Set static values or session variables as default values for columns
            while {getIngForm(query)}
          </Tooltip>
        );

        let presetStatus = '';
        if (presets.length > 1) {
          presetStatus = presets
            .map(p => p.key)
            .filter(p => p !== '')
            .join(', ');
        } else {
          presetStatus = 'no presets';
        }

        return (
          <CollapsibleToggle
            title={getSectionHeader(
              'Column presets',
              presetTooltip,
              presetStatus
            )}
            useDefaultTitleStyle
            testId={'toggle-presets-permission'}
          >
            <div
              className={sectionClasses}
              title={noPermissions ? noPermissionsMsg : ''}
            >
              <form className={styles.form_permission_insert_set_wrapper}>
                <div className={styles.permission_insert_set_wrapper}>
                  {getPresetValues()}
                </div>
              </form>
            </div>
          </CollapsibleToggle>
        );
      };

      const getAggregationSection = () => {
        if (query !== 'select') {
          return;
        }

        const handleClick = e => {
          dispatch(permToggleAllowAggregation(e.target.checked));
        };

        const aggregationAllowed = permissionsState.select
          ? permissionsState.select.allow_aggregations
          : false;

        const aggregationToolTip = (
          <Tooltip id="tooltip-aggregation">
            Allow queries with aggregate functions like sum, count, avg, max,
            min, etc
          </Tooltip>
        );

        const aggregationStatus = aggregationAllowed ? 'enabled' : 'disabled';

        return (
          <CollapsibleToggle
            title={getSectionHeader(
              'Aggregation queries permissions',
              aggregationToolTip,
              aggregationStatus
            )}
            useDefaultTitleStyle
            testId={'toggle-agg-permission'}
          >
            <div
              className={sectionClasses}
              title={noPermissions ? noPermissionsMsg : ''}
            >
              <div className="checkbox">
                <label>
                  <input
                    type="checkbox"
                    checked={aggregationAllowed}
                    value="toggle_aggregation"
                    onChange={handleClick}
                    disabled={noPermissions}
                    title={noPermissions ? noPermissionsMsg : ''}
                  />
                  Allow role <b>{permissionsState.role}</b> to make aggregation
                  queries
                </label>
              </div>
            </div>
          </CollapsibleToggle>
        );
      };

      const getClonePermsSection = () => {
        // const applySameSelected = e => {
        //   const isChecked = e.target.checked;
        //   const selectedRole = e.target.getAttribute('data-role');
        //   dispatch(permSetSameSelect(isChecked, selectedRole));
        // };

        const applySameBulk = () => {
          const confirmMessage = 'This will overwrite any existing permissions';
          const isOk = getConfirmation(confirmMessage);
          if (isOk) {
            dispatch(applySamePermissionsBulk(tableSchema));
          }
        };

        const applyToList = permissionsState.applySamePermissions;

        const disabledCloneMsg = 'No permissions are set';

        const getApplyToList = () => {
          const _applyToListHtml = [];

          const tableOptions = allSchemas.map(schema => schema.table_name);
          const actionsList = ['insert', 'select', 'update', 'delete'];

          const getApplyToRow = (applyTo, index) => {
            const getSelect = (type, options, value = '') => {
              const setApplyTo = e => {
                dispatch(permSetApplySamePerm(index, type, e.target.value));
              };

              const optionsList = options.map((option, i) => (
                <option key={i} value={option}>
                  {option}
                </option>
              ));

              return (
                <select
                  className={
                    styles.fkSelect +
                    ' ' +
                    styles.fkInEdit +
                    ' ' +
                    styles.add_mar_right +
                    ' input-sm form-control'
                  }
                  value={applyTo[type] || value || ''}
                  onChange={setApplyTo}
                  disabled={noPermissions}
                  title={noPermissions ? disabledCloneMsg : ''}
                >
                  <option disabled value="">
                    Select {type}
                  </option>
                  {optionsList}
                </select>
              );
            };

            const getRemoveIcon = () => {
              let _removeIcon = null;

              const removeApplyTo = () => {
                dispatch(permDelApplySamePerm(index));
              };

              if (applyTo.table && applyTo.role && applyTo.action) {
                _removeIcon = (
                  <i
                    className={`${styles.fontAwosomeClose} fa-lg fa fa-times`}
                    onClick={removeApplyTo}
                  />
                );
              }

              return _removeIcon;
            };

            return (
              <div key={index} className={styles.add_mar_bottom_mid}>
                {getSelect('table', tableOptions, permissionsState.table)}
                {getSelect('action', actionsList, permissionsState.query)}
                {getSelect('role', roleList)}
                {getRemoveIcon()}
              </div>
            );
          };

          applyToList.forEach((applyTo, i) => {
            _applyToListHtml.push(getApplyToRow(applyTo, i));
          });

          // add empty row (only if prev row is completely filled)
          const lastApplyTo = applyToList.length
            ? applyToList[applyToList.length - 1]
            : null;
          if (
            !lastApplyTo ||
            (lastApplyTo.table && lastApplyTo.action && lastApplyTo.role)
          ) {
            _applyToListHtml.push(getApplyToRow({}, applyToList.length));
          }

          return _applyToListHtml;
        };

        const applyToListHtml = getApplyToList();

        let clonePermissionsHtml = null;
        if (applyToListHtml.length) {
          const cloneToolTip = (
            <Tooltip id="tooltip-clone">
              Apply same permissions to other tables/roles/actions
            </Tooltip>
          );

          const validApplyToList = permissionsState.applySamePermissions.filter(
            applyTo => applyTo.table && applyTo.action && applyTo.role
          );

          clonePermissionsHtml = (
            <div>
              <hr />
              <CollapsibleToggle
                title={getSectionHeader('Clone permissions', cloneToolTip)}
                useDefaultTitleStyle
                testId={'toggle-clone-permission'}
              >
                <div
                  className={sectionClasses}
                  title={noPermissions ? disabledCloneMsg : ''}
                >
                  <div>Apply same permissions for:</div>
                  <div className={styles.add_mar_top_small}>
                    {applyToListHtml}
                  </div>
                  <div className={styles.add_mar_top}>
                    <b>Note:</b> While applying permissions for other tables,
                    the column permissions and presets will be ignored
                  </div>
                  <Button
                    onClick={applySameBulk}
                    className={styles.add_mar_top}
                    color="yellow"
                    size="sm"
                    disabled={!validApplyToList.length}
                  >
                    Save Permissions
                  </Button>
                </div>
              </CollapsibleToggle>
            </div>
          );
        }

        return clonePermissionsHtml;
      };

      const getButtonsSection = () => {
        const dispatchSavePermissions = () => {
          dispatch(permChangePermissions(permChangeTypes.save));
        };

        const dispatchRemoveAccess = () => {
          const confirmMessage =
            'This will permanently delete the currently set permissions';
          const isOk = getConfirmation(confirmMessage);
          if (isOk) {
            dispatch(permChangePermissions(permChangeTypes.delete));
          }
        };

        const getPermActionButton = (
          value,
          color,
          onClickFn,
          disabled,
          title
        ) => (
          <Button
            className={styles.add_mar_right}
            color={color}
            size="sm"
            onClick={onClickFn}
            disabled={disabled}
            title={title}
            data-test={`${value.split(' ').join('-')}-button`}
          >
            {value}
          </Button>
        );

        const rolePermissions = tableSchema.permissions.find(
          p => p.role_name === permissionsState.role
        );
        const currQueryPermissions = rolePermissions
          ? rolePermissions.permissions[permissionsState.query]
          : undefined;
        const newQueryPermissions = permissionsState[permissionsState.query];

        const applySameSelected = permissionsState.applySamePermissions.length;
        const permsChanged =
          JSON.stringify(newQueryPermissions) !==
          JSON.stringify(currQueryPermissions);

        const disableSave = applySameSelected || !permsChanged;
        const disableRemoveAccess = !currQueryPermissions;

        const saveButton = getPermActionButton(
          'Save Permissions',
          'yellow',
          dispatchSavePermissions,
          disableSave,
          !permsChanged ? 'No changes made' : ''
        );

        const removeAccessButton = getPermActionButton(
          'Delete Permissions',
          'red',
          dispatchRemoveAccess,
          disableRemoveAccess,
          disableRemoveAccess ? 'No permissions set' : ''
        );

        return (
          <div className={styles.add_mar_top + ' ' + styles.add_pad_left}>
            {saveButton}
            {removeAccessButton}
          </div>
        );
      };

      return (
        <div
          id={'permission-edit-section'}
          key={`${permissionsState.role}-${permissionsState.query}`}
          className={styles.activeEdit}
        >
          <div className={styles.editPermsHeading}>
            <span className={styles.add_mar_right}>
              <Button
                size="xs"
                onClick={dispatchCloseEdit}
                data-test={'close-button'}
              >
                Close
              </Button>
            </span>
            <span className={styles.add_mar_right}>
              Role: {permissionsState.role}
            </span>
            <span>Action: {permissionsState.query}</span>
          </div>
          <div>
            {getRowSection()}
            {getColumnSection()}
            {getAggregationSection()}
            {/*{getUpsertSection()}*/}
            {getPresetsSection('insert')}
            {getPresetsSection('update')}
            {getButtonsSection()}
            {getClonePermsSection()}
          </div>
        </div>
      );
    };

    /********************/

    const tSchema = allSchemas.find(
      t => t.table_name === tableName && t.table_schema === currentSchema
    );

    if (!tSchema) {
      return null;
    }

    let qTypes;
    if (tableType === 'table') {
      qTypes = ['insert', 'select', 'update', 'delete'];
    } else if (tableType === 'view') {
      qTypes = [];

      // Add insert/update permission if it is insertable/updatable as returned by pg
      if (tSchema.view_info) {
        if (
          tSchema.view_info.is_insertable_into === 'YES' ||
          tSchema.view_info.is_trigger_insertable_into === 'YES'
        ) {
          qTypes.push('insert');
        }

        qTypes.push('select'); // to maintain order

        if (tSchema.view_info.is_updatable === 'YES') {
          qTypes.push('update');
          qTypes.push('delete');
        } else {
          if (tSchema.view_info.is_trigger_updatable === 'YES') {
            qTypes.push('update');
          }

          if (tSchema.view_info.is_trigger_deletable === 'YES') {
            qTypes.push('delete');
          }
        }
      } else {
        qTypes.push('select');
      }
    }

    const allRolesList = getAllRoles(allSchemas);

    return (
      <div className={styles.container}>
        {getHeader(tSchema)}
        <br />
        <div className={styles.padd_left_remove}>
          <div className={`${styles.padd_remove} col-xs-12`}>
            <h4 className={styles.subheading_text}>Permissions</h4>
            {getPermissionsTable(tSchema, qTypes, allRolesList)}
            {getBulkSection(tSchema)}
            {getEditSection(tSchema, qTypes, allRolesList)}
          </div>
        </div>
        <div className={`${styles.fixed} hidden`}>
          {getAlertHtml(ongoingRequest, lastError, lastSuccess, lastFormError)}
        </div>
      </div>
    );
  }
}

Permissions.propTypes = {
  dispatch: PropTypes.func.isRequired,
  tableName: PropTypes.string.isRequired,
  tableType: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  currentSchema: PropTypes.string.isRequired,
  activeEdit: PropTypes.object.isRequired,
  permissionsState: PropTypes.object.isRequired,
  ongoingRequest: PropTypes.bool.isRequired,
  lastError: PropTypes.object,
  lastFormError: PropTypes.object,
  lastSuccess: PropTypes.bool,
};

const mapStateToProps = (state, ownProps) => ({
  tableName: ownProps.params.table,
  tableType: ownProps.route.tableType,
  allSchemas: state.tables.allSchemas,
  schemaList: state.tables.schemaList,
  migrationMode: state.main.migrationMode,
  currentSchema: state.tables.currentSchema,
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
  ...state.tables.modify,
});

const permissionsConnector = connect => connect(mapStateToProps)(Permissions);

export default permissionsConnector;
