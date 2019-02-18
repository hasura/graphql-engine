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
  getQueriesWithPermColumns,
  permChangeTypes,
  permOpenEdit,
  permAddTableSchemas,
  permSetFilter,
  permSetFilterSameAs,
  permToggleColumn,
  permToggleAllColumns,
  permAllowAll,
  permCloseEdit,
  permSetRoleName,
  permChangePermissions,
  permToggleAllowUpsert,
  permToggleAllowAggregation,
  permToggleModifyLimit,
  permCustomChecked,
  permRemoveRole,
  permSetBulkSelect,
  permRemoveMultipleRoles,
  permSetSameSelect,
  applySamePermissionsBulk,
  UPDATE_PERM_SET_KEY_VALUE,
  CREATE_NEW_SET_VAL,
  DELETE_SET_VAL,
  TOGGLE_PERM_SET_OPERATION_CHECK,
  setConfigValueType,
  X_HASURA_CONST,
} from './Actions';
import PermissionBuilder from './PermissionBuilder/PermissionBuilder';
import TableHeader from '../TableCommon/TableHeader';
import ViewHeader from '../TableBrowseRows/ViewHeader';
import { setTable, fetchViewInfoFromInformationSchema } from '../DataActions';
import { getIngForm, escapeRegExp } from '../utils';
import { allOperators, getLegacyOperator } from './PermissionBuilder/utils';
import semverCheck from '../../../../helpers/semver';
import Button from '../../Layout/Button/Button';

/* */
import EnhancedInput from '../../../Common/InputChecker/InputChecker';
/* */

class Permissions extends Component {
  constructor() {
    super();
    this.state = {};
    this.state.setOperations = {
      insert: {
        isChecked: false,
        columnTypeMap: {},
      },
      update: {
        isChecked: false,
        columnTypeMap: {},
      },
    };
    this.state.viewInfo = {};
    this.state.showAggregation = false;
    this.state.showInsertPrefix = false;
    this.state.showUpdatePresets = false;
  }
  componentDidMount() {
    if (this.props.serverVersion) {
      this.checkSemVer(this.props.serverVersion);
      this.checkPrefixVer(this.props.serverVersion);
      this.checkUpdatePresetVer(this.props.serverVersion);
    }
    this.props.dispatch({ type: RESET });
    const currentSchema = this.props.allSchemas.find(
      t => t.table_name === this.props.tableName
    );

    if (!currentSchema) {
      alert('Invalid schema');
      return;
    }

    this.props.dispatch(setTable(this.props.tableName));
    this.props
      .dispatch(
        fetchViewInfoFromInformationSchema(
          currentSchema.table_schema,
          this.props.tableName
        )
      )
      .then(r => {
        if (r.length > 0) {
          this.setState({ viewInfo: r[0] });
        }
      });
  }

  componentWillReceiveProps(nextProps) {
    if (nextProps.serverVersion !== this.props.serverVersion) {
      this.checkSemVer(nextProps.serverVersion);
      this.checkPrefixVer(nextProps.serverVersion);
      this.checkUpdatePresetVer(nextProps.serverVersion);
    }
  }

  onSetValueChange(e, queryType) {
    // Get the index of the changed value and if both key and value are set create one more object in set
    const inputNode = e.target;

    const indexId =
      inputNode && parseInt(inputNode.getAttribute('data-index-id'), 10);
    const prefixVal = inputNode && inputNode.getAttribute('data-prefix-val');
    const actionData = {};

    if (indexId >= 0) {
      actionData.key = 'value';
      actionData.value = (prefixVal || '') + inputNode.value;
      actionData.index = indexId;

      this.props.dispatch({
        type: UPDATE_PERM_SET_KEY_VALUE,
        data: { ...actionData, queryType },
      });
    }
  }

  onSetKeyChange = (e, queryType) => {
    // Get the index of the changed value and if both key and value are set create one more object in set
    const selectNode = e.target;
    const selectedOption = e.target.selectedOptions[0];

    const indexId =
      selectNode && parseInt(selectNode.getAttribute('data-index-id'), 10);
    const actionData = {};

    if (selectedOption && indexId >= 0) {
      actionData.key = 'key';
      actionData.value = selectNode.value;
      actionData.index = indexId;

      const columnType = selectedOption.getAttribute('data-column-type');

      this.props.dispatch({
        type: UPDATE_PERM_SET_KEY_VALUE,
        data: { ...actionData, queryType },
      });
      if (
        indexId + 1 ===
        this.props.permissionsState[queryType].localSet.length
      ) {
        this.addNewPresetColumn(queryType);
      }
      this.setState({
        ...this.state,
        setOperations: {
          ...this.state.setOperations,
          [queryType]: {
            ...this.state.setOperations[queryType],
            columnTypeMap: {
              ...this.state.setOperations[queryType].columnTypeMap,
              [indexId]: columnType,
            },
          },
        },
      });
    }
  };
  onSetTypeChange(e, queryType) {
    const selectNode = e.target;

    const indexId =
      selectNode && parseInt(selectNode.getAttribute('data-index-id'), 10);
    if (indexId >= 0) {
      // Clearing the stuff just to filter out errored cases
      const actionData = {};
      actionData.key = 'value';
      actionData.value = e.target.value === 'session' ? X_HASURA_CONST : '';
      actionData.index = indexId;

      this.props.dispatch({
        type: UPDATE_PERM_SET_KEY_VALUE,
        data: { ...actionData, queryType },
      });
    }
  }
  checkSemVer(version) {
    this.setState({
      showAggregation: semverCheck('aggregationPerm', version),
      showUpsertSection: !semverCheck('permHideUpsertSection', version),
    });
  }
  checkPrefixVer(version) {
    this.setState({
      showInsertPrefix: semverCheck('insertPrefix', version),
    });
  }
  checkUpdatePresetVer(version) {
    this.setState({
      showUpdatePresets: semverCheck('permUpdatePresets', version),
    });
  }
  deleteSetKeyVal(e, queryType) {
    const deleteIndex = parseInt(e.target.getAttribute('data-index-id'), 10);
    if (deleteIndex >= 0) {
      this.props.dispatch({
        type: DELETE_SET_VAL,
        data: {
          index: deleteIndex,
          queryType,
        },
      });
    }
  }
  addNewPresetColumn(queryType) {
    this.props.dispatch({ type: CREATE_NEW_SET_VAL, data: { queryType } });
  }
  togglePresetChecked(queryType) {
    this.props.dispatch({
      type: TOGGLE_PERM_SET_OPERATION_CHECK,
      data: { queryType },
    });
  }

  render() {
    const {
      dispatch,
      tableName,
      tableType,
      allSchemas,
      ongoingRequest,
      lastError,
      lastFormError,
      lastSuccess,
      permissionsState,
      migrationMode,
      currentSchema,
    } = this.props;
    const { showAggregation, showInsertPrefix, showUpdatePresets } = this.state;
    const styles = require('../TableModify/Modify.scss');

    const getAllRoles = (allTableSchemas) => {
      const _allRoles = [];

      allTableSchemas.forEach(tableSchema => {
        if (tableSchema.permissions) {
          tableSchema.permissions.forEach(p => {
            if (!_allRoles.includes(p.role_name)) {
              _allRoles.push(p.role_name);
            }
          });
        }
      });

      _allRoles.sort();

      return _allRoles;
    };

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

    const getEditLink = () => (
      <span className={styles.editPermissionLink}>
        <i className="fa fa-pencil" aria-hidden="true" />
      </span>
    );

    const getRoleQueryPermission = (
      tableSchema,
      role,
      queryType,
      permissionsSymbols
    ) => {
      let _permission;

      const rolePermissions = {};
      tableSchema.permissions.forEach(
        p => (rolePermissions[p.role_name] = p.permissions)
      );

      if (role === 'admin') {
        _permission = permissionsSymbols.fullAccess;
      } else if (!Object.keys(rolePermissions).includes(role)) {
        _permission = permissionsSymbols.noAccess;
      } else {
        const permissions = rolePermissions[role][queryType];

        /* eslint-disable no-fallthrough */
        if (permissions) {
          let checkColumns = false;
          let filterKey;
          switch (queryType) {
            case 'select':
            case 'update':
              checkColumns = true;
            case 'delete':
              filterKey = 'filter';
            case 'insert':
              filterKey = filterKey || 'check';

              if (JSON.stringify(permissions[filterKey]) === '{}') {
                if (
                  checkColumns &&
                  !permissions.columns.includes('*') &&
                  permissions.columns.length !== tableSchema.columns.length
                ) {
                  _permission = permissionsSymbols.partialAccess;
                } else {
                  _permission = permissionsSymbols.fullAccess;
                }
              } else {
                _permission = permissionsSymbols.partialAccess;
              }
              break;
            default:
              _permission = permissionsSymbols.noAccess;
          }
        } else {
          _permission = permissionsSymbols.noAccess;
        }
        /* eslint-enable no-fallthrough */
      }

      return _permission;
    };

    const getPermissionsTableRow = (
      tableSchema,
      role,
      queryTypes,
      permissionsSymbols,
      permsState,
      isNewPerm
    ) => {
      const dispatchOpenEdit = queryType => () => {
        if (isNewPerm && permsState.newRole !== '') {
          dispatch(permOpenEdit(tableSchema, permsState.newRole, queryType));
        } else if (role !== '') {
          const allowInsertPermColumns = semverCheck('insertPermRestrictColumns', this.props.serverVersion);

          dispatch(permOpenEdit(tableSchema, role, queryType, allowInsertPermColumns));
        } else {
          document.getElementById('newRoleInput').focus();
        }
      };

      const dispatchCloseEdit = () => {
        dispatch(permCloseEdit());
      };

      const dispatchRoleNameChange = e => {
        dispatch(permSetRoleName(e.target.value));
      };

      const dispatchBulkSelect = e => {
        const isChecked = e.target.checked;
        const selectedRole = e.target.getAttribute('data-role');
        dispatch(permSetBulkSelect(isChecked, selectedRole));
      };

      const dispatchDeletePermission = () => {
        const isConfirm = window.confirm(
          'Are you sure you want to delete the permission for role ' + role + '?'
        );
        if (isConfirm) {
          dispatch(permRemoveRole(tableSchema, role));
        }
      };

      const _permissionsRowHtml = [];
      if (role === 'admin' || role === '') {
        _permissionsRowHtml.push(<td key={-1} />);
      } else {
        const bulkSelect = permsState.bulkSelect;
        const currentInputSelection = (
          <input
            onChange={dispatchBulkSelect}
            checked={bulkSelect.filter(e => e === role).length}
            data-role={role}
            className={styles.bulkSelect}
            type="checkbox"
          />
        );
        _permissionsRowHtml.push(
          <td key={-1}>
            <div>
              {currentInputSelection}
              <i
                onClick={dispatchDeletePermission}
                className={styles.permissionDelete + ' fa fa-trash'}
                aria-hidden="true"
              />
            </div>
          </td>
        );
      }

      if (isNewPerm) {
        _permissionsRowHtml.push(
          <td key={-2}>
            <input
              id="newRoleInput"
              className={`form-control ${styles.newRoleInput}`}
              onChange={dispatchRoleNameChange}
              type="text"
              placeholder="Enter new role"
              value={permsState.newRole}
              data-test="role-textbox"
            />
          </td>
        );
      } else {
        _permissionsRowHtml.push(<td key={-2}>{role}</td>);
      }

      queryTypes.forEach((queryType, i) => {
        const isEditAllowed = role !== 'admin';
        const isCurrEdit =
          permsState.role === role && permsState.query === queryType;

        let editLink = '';
        let className = '';
        let onClick = () => {};
        if (isEditAllowed) {
          editLink = getEditLink();

          className += styles.clickableCell;
          onClick = dispatchOpenEdit(queryType);
          if (isCurrEdit) {
            onClick = dispatchCloseEdit;
            className += ` ${styles.currEdit}`;
          }
        }

        if (role === '') {
          className += ` ${styles.newRoleTd}`;
        }

        _permissionsRowHtml.push(
          <td
            key={i}
            className={className}
            onClick={onClick}
            title="Click to edit permissions"
            data-test={`${role}-${queryType}`}
          >
            {getRoleQueryPermission(
              tableSchema,
              role,
              queryType,
              permissionsSymbols
            )}
            {editLink}
          </td>
        );
      });

      return _permissionsRowHtml;
    };

    const getPermissionsTableBody = (
      tableSchema,
      queryTypes,
      permissionsSymbols,
      permsState,
      roleList
    ) => {
      const _permissionsRowsHtml = [];

      // add admin to roles
      const _roleList = ['admin'].concat(roleList);

      _roleList.forEach((role, i) => {
        _permissionsRowsHtml.push(
          <tr key={i}>
            {getPermissionsTableRow(
              tableSchema,
              role,
              queryTypes,
              permissionsSymbols,
              permsState
            )}
          </tr>
        );
      });

      // new role row
      _permissionsRowsHtml.push(
        <tr key="newPerm">
          {getPermissionsTableRow(
            tableSchema,
            '',
            queryTypes,
            permissionsSymbols,
            permsState,
            true
          )}
        </tr>
      );

      return <tbody>{_permissionsRowsHtml}</tbody>;
    };

    const getPermissionsTableHead = queryTypes => {
      const _permissionsHead = [];

      _permissionsHead.push(<td key={-1}>Actions</td>);
      _permissionsHead.push(<td key={-2}>Role</td>);

      queryTypes.forEach((queryType, i) => {
        _permissionsHead.push(<td key={i}>{queryType}</td>);
      });

      return (
        <thead>
          <tr>{_permissionsHead}</tr>
        </thead>
      );
    };

    const getPermissionsLegend = permissionsSymbols => (
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
      let showNote = false;
      if (
        tableType === 'view' &&
        !(
          this.state.viewInfo &&
          'is_insertable_into' in this.state.viewInfo &&
          this.state.viewInfo.is_insertable_into === 'YES'
        ) &&
        !(
          this.state.viewInfo &&
          'is_updatable' in this.state.viewInfo &&
          this.state.viewInfo.is_updatable === 'YES'
        )
      ) {
        showNote = true;
      }

      return showNote ? (
        <div className={styles.permissionsLegend}>
          <i className="fa fa-question-circle" aria-hidden="true" />
          &nbsp; You cannot insert/update into this view
        </div>
      ) : (
        ''
      );
    };

    const getPermissionsTable = (tableSchema, queryTypes, permsState, roleList) => {
      const permissionsSymbols = {
        fullAccess: <i className="fa fa-check" aria-hidden="true" />,
        noAccess: <i className="fa fa-times" aria-hidden="true" />,
        partialAccess: <i className="fa fa-filter" aria-hidden="true" />,
      };

      return (
        <div>
          {getPermissionsLegend(permissionsSymbols)}
          {getViewPermissionNote()}
          <table className={`table table-bordered ${styles.permissionsTable}`}>
            {getPermissionsTableHead(queryTypes)}
            {getPermissionsTableBody(
              tableSchema,
              queryTypes,
              permissionsSymbols,
              permsState,
              roleList
            )}
          </table>
        </div>
      );
    };
    const getUpsertSection = permsState => {
      if (!this.state.showUpsertSection) {
        return null;
      }

      let _upsertSection;
      const query = permsState.query;

      if (query === 'insert') {
        const dispatchToggleAllowUpsert = checked => {
          dispatch(permToggleAllowUpsert(checked));
        };

        const upsertAllowed = permsState.insert
          ? permsState.insert.allow_upsert
          : false;

        // Upsert Tooltip
        const upsertToolTip = (
          <Tooltip id="tooltip-upsert">
            Upsert updates a row if it already exists, otherwise inserts it
          </Tooltip>
        );

        _upsertSection = (
          <div>
            <div className="radio">
              <label>
                <div className={styles.center_radio_label_input}>
                  <input
                    type="checkbox"
                    disabled={!permsState.insert}
                    checked={upsertAllowed}
                    value="toggle_upsert"
                    onChange={e => dispatchToggleAllowUpsert(e.target.checked)}
                  />
                  <span className={styles.mar_left}>
                    Allow role '{permsState.role}' to make upsert queries &nbsp;
                    <OverlayTrigger placement="right" overlay={upsertToolTip}>
                      <i className="fa fa-question-circle" aria-hidden="true" />
                    </OverlayTrigger>
                  </span>
                </div>
              </label>
            </div>
          </div>
        );
      }
      return _upsertSection;
    };

    const getInsertSetPermission = (tableSchema, permsState) => {
      const query = permsState.query;

      if (query === 'insert') {
        const insertState = permsState.insert;
        const { columns } = tableSchema;
        const isSetValues = !!(
          insertState &&
          'localSet' in insertState &&
          insertState.isSetConfigChecked
        );
        const insertSetTooltip = (
          <Tooltip id="tooltip-insert-set-operations">
            Preset values for columns for this role. Set static values or use
            session variables.
          </Tooltip>
        );

        const setOptions =
          insertState && insertState.localSet && insertState.localSet.length > 0
            ? insertState.localSet.map((s, i) => {
              return (
                <div className={styles.insertSetConfigRow} key={i}>
                  <div
                    className={
                      styles.display_inline +
                        ' ' +
                        styles.add_mar_right +
                        ' ' +
                        styles.input_element_wrapper
                    }
                  >
                    <select
                      className="input-sm form-control"
                      value={s.key}
                      onChange={e => this.onSetKeyChange(e, 'insert')}
                      data-index-id={i}
                      data-test={'column-presets-column-' + i}
                    >
                      <option value="" disabled>
                          Column Name
                      </option>
                      {columns && columns.length > 0
                        ? columns.map((c, key) => (
                          <option
                            value={c.column_name}
                            data-column-type={c.data_type}
                            key={key}
                          >
                            {c.column_name}
                          </option>
                        ))
                        : null}
                    </select>
                  </div>
                  <div
                    className={
                      styles.display_inline +
                        ' ' +
                        styles.add_mar_right +
                        ' ' +
                        styles.input_element_wrapper
                    }
                  >
                    <select
                      className="input-sm form-control"
                      onChange={e => this.onSetTypeChange(e, 'insert')}
                      data-index-id={i}
                      data-test={'column-presets-type-' + i}
                      value={setConfigValueType(s.value) || ''}
                    >
                      <option value="" disabled>
                          Select Preset Type
                      </option>
                      <option value="static">static</option>
                      <option value="session">from session variable</option>
                    </select>
                  </div>
                  <div
                    className={
                      styles.display_inline +
                        ' ' +
                        styles.add_mar_right +
                        ' ' +
                        styles.input_element_wrapper
                    }
                  >
                    {setConfigValueType(s.value) === 'session' ? (
                      <InputGroup>
                        <InputGroup.Addon>X-Hasura-</InputGroup.Addon>
                        <input
                          className={'input-sm form-control '}
                          placeholder="column_value"
                          value={s.value.slice(X_HASURA_CONST.length)}
                          onChange={e => this.onSetValueChange(e, 'insert')}
                          data-test={'column-presets-value-' + i}
                          data-index-id={i}
                          data-prefix-val={X_HASURA_CONST}
                        />
                      </InputGroup>
                    ) : (
                      <EnhancedInput
                        placeholder="column_value"
                        type={
                          i in this.state.setOperations.insert.columnTypeMap
                            ? this.state.setOperations.insert.columnTypeMap[i]
                            : ''
                        }
                        value={s.value}
                        onChange={e => this.onSetValueChange(e, 'insert')}
                        data-test={'column-presets-value-' + i}
                        indexId={i}
                        data-prefix-val={X_HASURA_CONST}
                      />
                    )}
                  </div>
                  {setConfigValueType(s.value) === 'session' ? (
                    <div
                      className={
                        styles.display_inline +
                          ' ' +
                          styles.add_mar_right +
                          ' ' +
                          styles.input_element_wrapper +
                          ' ' +
                          styles.e_g_text
                      }
                    >
                        e.g. X-Hasura-User-Id
                    </div>
                  ) : (
                    <div
                      className={
                        styles.display_inline +
                          ' ' +
                          styles.add_mar_right +
                          ' ' +
                          styles.input_element_wrapper +
                          ' ' +
                          styles.e_g_text
                      }
                    >
                        e.g. false, 1, some-text
                    </div>
                  )}
                  {i !== insertState.localSet.length - 1 ? (
                    <div
                      className={
                        styles.display_inline +
                          ' ' +
                          styles.add_mar_right +
                          ' ' +
                          styles.input_element_wrapper
                      }
                    >
                      <i
                        className="fa-lg fa fa-times"
                        onClick={e => this.deleteSetKeyVal(e, 'insert')}
                        data-index-id={i}
                      />
                    </div>
                  ) : (
                    <div
                      className={
                        styles.display_inline +
                          ' ' +
                          styles.add_mar_right +
                          ' ' +
                          styles.input_element_wrapper
                      }
                    />
                  )}
                </div>
              );
            })
            : null;

        return (
          <div
            className={
              styles.editPermissionsSection + ' ' + styles.removePadding
            }
          >
            <form className={styles.form_permission_insert_set_wrapper}>
              <div className={styles.permission_insert_set_wrapper}>
                <div className={styles.configure_insert_set_checkbox}>
                  <label>
                    <div
                      className={styles.center_radio_label_input}
                      onClick={() => this.togglePresetChecked('insert')}
                      disabled={!permsState.insert}
                    >
                      <div className={styles.chevron_mar_right}>
                        <i
                          className={
                            isSetValues
                              ? 'fa fa-chevron-down'
                              : 'fa fa-chevron-right'
                          }
                          data-test="toggle-column-presets"
                        />
                      </div>
                      <span>
                        Configure column presets &nbsp;
                        <OverlayTrigger
                          placement="right"
                          overlay={insertSetTooltip}
                        >
                          <i
                            className="fa fa-question-circle"
                            aria-hidden="true"
                          />
                        </OverlayTrigger>
                      </span>
                    </div>
                  </label>
                </div>
                {isSetValues && setOptions}
              </div>
            </form>
          </div>
        );
      }
      return null;
    };

    const getUpdateSetSection = (tableSchema, permsState) => {
      const query = permsState.query;
      if (query === 'update') {
        const updateState = permsState.update;
        const { columns } = tableSchema;
        const isSetValues = !!(
          updateState &&
          'localSet' in updateState &&
          updateState.isSetConfigChecked
        );
        const updateSetTooltip = (
          <Tooltip id="tooltip-update-set-operations">
            Preset values for columns for this role. Set static values or use
            session variables.
          </Tooltip>
        );

        const setOptions =
          updateState && updateState.localSet && updateState.localSet.length > 0
            ? updateState.localSet.map((s, i) => {
              return (
                <div className={styles.insertSetConfigRow} key={i}>
                  <div
                    className={
                      styles.display_inline +
                        ' ' +
                        styles.add_mar_right +
                        ' ' +
                        styles.input_element_wrapper
                    }
                  >
                    <select
                      className="input-sm form-control"
                      value={s.key}
                      data-test={'column-presets-column-' + i}
                      onChange={e => this.onSetKeyChange(e, 'update')}
                      data-index-id={i}
                    >
                      <option value="" disabled>
                          Column Name
                      </option>
                      {columns && columns.length > 0
                        ? columns.map((c, key) => (
                          <option
                            value={c.column_name}
                            data-column-type={c.data_type}
                            key={key}
                          >
                            {c.column_name}
                          </option>
                        ))
                        : null}
                    </select>
                  </div>
                  <div
                    className={
                      styles.display_inline +
                        ' ' +
                        styles.add_mar_right +
                        ' ' +
                        styles.input_element_wrapper
                    }
                  >
                    <select
                      className="input-sm form-control"
                      onChange={e => this.onSetTypeChange(e, 'update')}
                      data-index-id={i}
                      data-test={'column-presets-type-' + i}
                      value={setConfigValueType(s.value) || ''}
                    >
                      <option value="" disabled>
                          Select Preset Type
                      </option>
                      <option value="static">static</option>
                      <option value="session">from session variable</option>
                    </select>
                  </div>
                  <div
                    className={
                      styles.display_inline +
                        ' ' +
                        styles.add_mar_right +
                        ' ' +
                        styles.input_element_wrapper
                    }
                  >
                    {setConfigValueType(s.value) === 'session' ? (
                      <InputGroup>
                        <InputGroup.Addon>X-Hasura-</InputGroup.Addon>
                        <input
                          className={'input-sm form-control '}
                          placeholder="column_value"
                          value={s.value.slice(X_HASURA_CONST.length)}
                          onChange={e => this.onSetValueChange(e, 'update')}
                          data-index-id={i}
                          data-prefix-val={X_HASURA_CONST}
                          data-test={'column-presets-value-' + i}
                        />
                      </InputGroup>
                    ) : (
                      <EnhancedInput
                        placeholder="column_value"
                        type={
                          i in this.state.setOperations.update.columnTypeMap
                            ? this.state.setOperations.update.columnTypeMap[i]
                            : ''
                        }
                        value={s.value}
                        onChange={e => this.onSetValueChange(e, 'update')}
                        indexId={i}
                        data-prefix-val={X_HASURA_CONST}
                        data-test={'column-presets-value-' + i}
                      />
                    )}
                  </div>
                  {setConfigValueType(s.value) === 'session' ? (
                    <div
                      className={
                        styles.display_inline +
                          ' ' +
                          styles.add_mar_right +
                          ' ' +
                          styles.input_element_wrapper +
                          ' ' +
                          styles.e_g_text
                      }
                    >
                        e.g. X-Hasura-User-Id
                    </div>
                  ) : (
                    <div
                      className={
                        styles.display_inline +
                          ' ' +
                          styles.add_mar_right +
                          ' ' +
                          styles.input_element_wrapper +
                          ' ' +
                          styles.e_g_text
                      }
                    >
                        e.g. false, 1, some-text
                    </div>
                  )}
                  {i !== updateState.localSet.length - 1 ? (
                    <div
                      className={
                        styles.display_inline +
                          ' ' +
                          styles.add_mar_right +
                          ' ' +
                          styles.input_element_wrapper
                      }
                    >
                      <i
                        className="fa-lg fa fa-times"
                        onClick={e => this.deleteSetKeyVal(e, 'update')}
                        data-index-id={i}
                      />
                    </div>
                  ) : (
                    <div
                      className={
                        styles.display_inline +
                          ' ' +
                          styles.add_mar_right +
                          ' ' +
                          styles.input_element_wrapper
                      }
                    />
                  )}
                </div>
              );
            })
            : null;
        return (
          <div
            className={
              styles.editPermissionsSection + ' ' + styles.removePadding
            }
          >
            <form className={styles.form_permission_insert_set_wrapper}>
              <div className={styles.permission_insert_set_wrapper}>
                <div
                  className={styles.configure_insert_set_checkbox}
                  onClick={() => this.togglePresetChecked('update')}
                  disabled={!permsState.update}
                >
                  <label>
                    <div className={styles.center_radio_label_input}>
                      <div className={styles.chevron_mar_right}>
                        <i
                          className={
                            isSetValues
                              ? 'fa fa-chevron-down'
                              : 'fa fa-chevron-right'
                          }
                          data-test="toggle-column-presets"
                        />
                      </div>
                      <span>
                        Configure column presets &nbsp;
                        <OverlayTrigger
                          placement="right"
                          overlay={updateSetTooltip}
                        >
                          <i
                            className="fa fa-question-circle"
                            aria-hidden="true"
                          />
                        </OverlayTrigger>
                      </span>
                    </div>
                  </label>
                </div>
                {isSetValues && setOptions}
              </div>
            </form>
          </div>
        );
      }
      return null;
    };
    const getAggregationSection = permsState => {
      let _aggregationSection = '';
      const query = permsState.query;

      if (query === 'select') {
        const dispatchToggleAllowAggregation = checked => {
          dispatch(permToggleAllowAggregation(checked));
        };

        const aggregationAllowed = permsState.select
          ? permsState.select.allow_aggregations
          : false;

        // Aggregation Tooltip
        const aggregationToolTip = (
          <Tooltip id="tooltip-aggregation">
            Allow queries with aggregate functions like sum, count, avg, max,
            min, etc
          </Tooltip>
        );

        _aggregationSection = (
          <div className={styles.mar_small_neg_left_1}>
            <div className="radio">
              <label>
                <input
                  type="checkbox"
                  disabled={!permsState.select}
                  checked={aggregationAllowed}
                  value="toggle_aggregation"
                  onChange={e =>
                    dispatchToggleAllowAggregation(e.target.checked)
                  }
                />
                <span className={styles.mar_small_left}>
                  Allow role '{permsState.role}' to make aggregation queries
                  &nbsp;
                  <OverlayTrigger
                    placement="right"
                    overlay={aggregationToolTip}
                  >
                    <i className="fa fa-question-circle" aria-hidden="true" />
                  </OverlayTrigger>
                </span>
              </label>
            </div>
          </div>
        );
      }

      return _aggregationSection;
    };

    const getColumnList = (tableSchema, permsState) => {
      const _columnList = [];

      const query = permsState.query;

      const dispatchToggleColumn = e => {
        const column = e.target.value;
        dispatch(permToggleColumn(column));
      };

      tableSchema.columns.forEach((colObj, i) => {
        const column = colObj.column_name;
        const checked = permsState[query] ? permsState[query].columns.includes(column) : false;

        _columnList.push(
          <div key={i} className={styles.columnListElement}>
            <div className="checkbox">
              <label>
                <input
                  type="checkbox"
                  checked={checked}
                  value={column}
                  onChange={dispatchToggleColumn}
                />
                {column}
              </label>
            </div>
          </div>
        );
      });

      _columnList.push(
        <div key={-1} className={styles.clear_fix} />
      );

      return _columnList;
    };

    const getRelationshipsMsg = (tableSchema) => {
      let _relationshipsMsg = '';

      const relationships = tableSchema.relationships.map(relObj => relObj.rel_name);

      if (relationships.length) {
        _relationshipsMsg = (
          <div className={styles.add_mar_top_small}>
            For <b>relationship{relationships.length !== 1 ? 's' : ''}</b>: <i>{relationships.join(', ')}</i>.
            <span className={styles.mar_left}>Set permissions on the corresponding table/view.</span>
          </div>
        );
      }

      return _relationshipsMsg;
    };

    const getColumnSection = (tableSchema, permsState) => {
      const { serverVersion } = this.props;

      let _columnSection = '';

      const query = permsState.query;

      const allowInsertPermColumns = semverCheck('insertPermRestrictColumns', serverVersion);
      const queriesWithPermColumns = getQueriesWithPermColumns(allowInsertPermColumns);

      if (queriesWithPermColumns.includes(query)) {
        const dispatchToggleAllColumns = () => {
          const allColumns = tableSchema.columns.map(c => c.column_name);
          dispatch(permToggleAllColumns(allColumns));
        };

        let accessText;
        if (query === 'insert') {
          accessText = 'input for';
        } else if (query === 'select') {
          accessText = 'access to';
        } else {
          accessText = 'updates to';
        }

        _columnSection = (
          <div className={styles.editPermissionsSection}>
            <div>
              Allow {accessText} these <b>columns</b>:
              <span
                className={styles.toggleAll}
                onClick={dispatchToggleAllColumns}
              >
                Toggle all
              </span>
            </div>

            { getColumnList(tableSchema, permsState) }

            { getRelationshipsMsg(tableSchema) }

          </div>
        );
      }

      return _columnSection;
    };

    const getLimitSection = permsState => {
      const dispatchLimit = limit => {
        const parsedLimit = parseInt(limit, 10);
        dispatch(permToggleModifyLimit(parsedLimit));
      };
      const query = permsState.query;
      const limitValue =
        permsState.select && permsState.select.limit
          ? permsState.select.limit
          : '';

      if (query === 'select') {
        const _limitSection = (
          <div className={styles.mar_small_neg_left}>
            <div className="radio">
              <label>
                <span className={styles.mar_small_left}>
                  Limit the no of rows
                </span>
              </label>
              <input
                className={
                  styles.mar_small_left + ' form-control ' + styles.limitInput
                }
                value={limitValue}
                onChange={e => dispatchLimit(e.target.value)}
                type="number"
                min="0"
              />
            </div>
            <div className={styles.clear_fix} />
          </div>
        );
        return _limitSection;
      }
    };

    const getFilterQueries = (queryTypes, permsState) => {
      const _filterQueries = {};

      queryTypes.forEach(queryType => {
        if (queryType === permsState.query) {
          return;
        }

        const queryFilterKey = queryType === 'insert' ? 'check' : 'filter';

        let queryFilterString = '';
        if (permsState[queryType]) {
          queryFilterString = JSON.stringify(
            permsState[queryType][queryFilterKey]
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

    const isUniqueFilter = (queryTypes, permsState, filterString) => {
      const filterQueries = getFilterQueries(queryTypes, permsState);

      return (
        filterString !== '' &&
        filterString !== '{}' &&
        !filterQueries[filterString]
      );
    };

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

    const getFilterOptions = (queryTypes, permsState, filterString) => {
      const dispatchAllowAll = () => {
        dispatch(permAllowAll());
      };

      const dispatchSetFilterSameAs = filter => () => {
        dispatch(permSetFilterSameAs(JSON.parse(filter)));
      };

      const dispatchCustomChecked = () => {
        dispatch(permCustomChecked());
      };

      const _filterOptionsSection = [];

      const filterQueries = getFilterQueries(queryTypes, permsState);

      let isSelected;
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

      // Add allow all option
      isSelected = !permsState.custom_checked && filterString === '{}';
      let allowAllQueryInfo = '';
      if (filterQueries['{}']) {
        allowAllQueryInfo = (
          <span>
            (Same as <b>{filterQueries['{}'].join(', ')}</b>)
          </span>
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

      // Add other query options
      Object.keys(filterQueries).forEach((filter, i) => {
        isSelected = !permsState.custom_checked && filterString === filter;
        if (filter === '{}') {
          return;
        }
        const queries = filterQueries[filter].join(', ');
        const queryLabel = (
          <span data-test="mutual-check">
            With same checks as <b>{queries}</b>
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

      // Add custom option
      isSelected =
        permsState.custom_checked ||
        isUniqueFilter(queryTypes, permsState, filterString);
      const dispatchFuncSetFilter = filter => permSetFilter(JSON.parse(filter));
      const dispatchFuncAddTableSchemas = schemaNames =>
        permAddTableSchemas(schemaNames);
      const customCheckToolTip = (
        <Tooltip id="tooltip-custom-check">
          Create custom check using permissions builder
        </Tooltip>
      );
      const customChecklabel = (
        <span data-test="custom-check">
          With custom check: &nbsp;
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
            dispatchFuncAddTableSchemas={dispatchFuncAddTableSchemas}
            tableName={tableName}
            schemaName={currentSchema}
            allTableSchemas={permissionsState.tableSchemas}
            filter={filterString}
            dispatch={dispatch}
            key={-4}
          />
        );
      }

      return _filterOptionsSection;
    };

    const getRowSection = (queryTypes, permsState) => {
      const query = permsState.query;

      const filterKey = query === 'insert' ? 'check' : 'filter';

      let filterString = '';
      if (permsState[query]) {
        filterString = JSON.stringify(permsState[query][filterKey]);
      }

      // replace legacy operator values
      allOperators.forEach(operator => {
        const currentString = '"' + operator + '"';
        const legacyString = '"' + getLegacyOperator(operator) + '"';

        filterString = filterString.replace(
          new RegExp(escapeRegExp(legacyString), 'g'),
          currentString
        );
      });

      return (
        <div className={styles.editPermissionsSection}>
          Allow {getIngForm(permsState.query)} <b>rows</b> for role '
          {permsState.role}
          ':
          <div>{getFilterOptions(queryTypes, permsState, filterString)}</div>
        </div>
      );
    };

    const getBulkApplySection = (tableSchema, permsState, roleList) => {
      const applySameSelected = e => {
        const isChecked = e.target.checked;
        const selectedRole = e.target.getAttribute('data-role');
        dispatch(permSetSameSelect(isChecked, selectedRole));
      };

      const applySameBulk = () => {
        if (window.confirm('Are you sure?')) {
          dispatch(applySamePermissionsBulk(tableSchema));
        }
      };

      const rolePermissions = tableSchema.permissions.find(
        p => p.role_name === permsState.role
      );
      const currQueryPermissions = rolePermissions ? rolePermissions.permissions[permsState.query] : null;

      const roleListHtml = [];
      if (currQueryPermissions) {
        roleList.forEach(role => {
          if (role !== permsState.role) {
            roleListHtml.push(
              <div
                key={role}
                className={styles.display_inline + ' checkbox ' + styles.add_mar_right}
              >
                <label>
                  <input
                    data-role={role}
                    onChange={applySameSelected}
                    className={
                      'form-control ' +
                      styles.samePermissionRole +
                      ' ' +
                      styles.add_mar_small
                    }
                    type="checkbox"
                  />
                  {role}
                </label>
              </div>
            );
          }
        });
      }

      let applyBulkPermissions = null;
      if (roleListHtml.length) {
        applyBulkPermissions = (
          <div className={styles.editPermissionsSection + ' ' + styles.removePadding}>
            <hr />
            <div>Apply same {permsState.query} permissions to other roles:</div>
            <div className={styles.add_mar_top_small}>{roleListHtml}</div>
            <Button
              onClick={applySameBulk}
              className={styles.bulkApplyBtn}
              color="white"
              size="sm"
              disabled={!permsState.applySamePermissions.length}
            >
              Save permissions
            </Button>
          </div>
        );
      }

      return applyBulkPermissions;
    };

    const getButton = (value, color, onClickFn, disabled) => (
      <Button
        className={styles.editActionButton}
        color={color}
        size="sm"
        onClick={onClickFn}
        disabled={disabled}
        data-test={`${value.split(' ').join('-')}-button`}
      >
        {value}
      </Button>
    );

    const getButtonsSection = (tableSchema, permsState) => {
      const dispatchSavePermissions = () => {
        dispatch(permChangePermissions(permChangeTypes.save));
      };

      const dispatchRemoveAccess = () => {
        const isOk = confirm('Are you sure?');
        if (isOk) {
          dispatch(permChangePermissions(permChangeTypes.delete));
        }
      };

      const dispatchCloseEdit = () => {
        dispatch(permCloseEdit());
      };

      const rolePermissions = tableSchema.permissions.find(
        p => p.role_name === permsState.role
      );
      const currQueryPermissions = rolePermissions
        ? rolePermissions.permissions[permsState.query]
        : undefined;
      const newQueryPermissions = permsState[permsState.query];

      const disableSave = permsState.applySamePermissions.length ||
        (JSON.stringify(newQueryPermissions) === JSON.stringify(currQueryPermissions));
      const disableRemoveAccess = !currQueryPermissions;

      const saveButton = getButton(
        'Save permissions',
        'yellow',
        dispatchSavePermissions,
        disableSave
      );

      const removeAccessButton = getButton(
        'Remove',
        'red',
        dispatchRemoveAccess,
        disableRemoveAccess
      );

      const closeButton = getButton('Close', 'white', dispatchCloseEdit);

      return (
        <div className={styles.editPermissionsSection}>
          {saveButton}
          {removeAccessButton}
          {closeButton}
        </div>
      );
    };

    const getEditPermissions = (tableSchema, queryTypes, permsState, roleList) => (
      <div className={styles.activeEdit}>
        <div className={styles.editPermissionsHeading}>
          Role: {permsState.role}
          &nbsp;&nbsp;&nbsp;Query: {permsState.query}
        </div>
        <hr className={styles.remove_margin} />
        <div>
          {getRowSection(queryTypes, permsState)}
          {getColumnSection(tableSchema, permsState)}
          {showAggregation && getAggregationSection(permsState)}
          {getLimitSection(permsState)}
          {getUpsertSection(permsState)}
          {showInsertPrefix && getInsertSetPermission(tableSchema, permsState)}
          {showUpdatePresets && getUpdateSetSection(tableSchema, permsState)}
          {getButtonsSection(tableSchema, permsState)}
          {getBulkApplySection(tableSchema, permsState, roleList)}
        </div>
      </div>
    );

    const getEditSection = (tableSchema, queryTypes, permsState, roleList) => {
      let _editSection = '';

      if (permsState.role && permsState.query) {
        _editSection = getEditPermissions(tableSchema, queryTypes, permsState, roleList);
      }

      return _editSection;
    };

    const getBulkSection = (tableSchema, queryTypes, permsState) => {
      if (!permsState.bulkSelect.length) {
        return null;
      }
      // const currentPermissions = tableSchema.permissions;
      const bulkSelect = permsState.bulkSelect;
      const handleBulkRemoveClick = () => {
        if (window.confirm('Are you sure?')) {
          dispatch(permRemoveMultipleRoles(tableSchema));
        }
      };

      return (
        <div className={styles.activeEdit}>
          <div className={styles.editPermissionsHeading}>
            Apply Bulk Actions
          </div>
          <hr className={styles.remove_margin} />
          <div className={styles.padd_top + ' ' + styles.padd_bottom}>
            <span className={styles.selectedRoles + ' ' + styles.add_pad_right}>
              Selected Roles
            </span>
            {bulkSelect.map(r => {
              return (
                <span key={r} className={styles.add_pad_right}>
                  <b>{r}</b>{' '}
                </span>
              );
            })}
          </div>
          <div className={styles.padd_bottom}>
            <Button onClick={handleBulkRemoveClick} color="red" size="sm">
              Remove Permissions
            </Button>
          </div>
        </div>
      );
    };

    /*
    const getEditRoleSection = (tableSchema, permsState) => {
      let _editSection = '';

      if (permsState.role && permsState.query) {
        _editSection = getEditPermissions(tableSchema, permsState);
      }

      return _editSection;
    };
    */

    const getHeader = tableSchema => {
      let _header;

      const isView = tableSchema.detail.table_type !== 'BASE TABLE';
      if (isView) {
        _header = (
          <ViewHeader
            dispatch={dispatch}
            tableName={tableName}
            tabName="permissions"
            migrationMode={migrationMode}
            currentSchema={currentSchema}
          />
        );
      } else {
        _header = (
          <TableHeader
            dispatch={dispatch}
            tableName={tableName}
            tabName="permissions"
            migrationMode={migrationMode}
            currentSchema={currentSchema}
          />
        );
      }

      return _header;
    };

    /********************/

    let qTypes;
    if (tableType === 'table') {
      qTypes = ['insert', 'select', 'update', 'delete'];
    } else if (tableType === 'view') {
      qTypes = [];
      // Add insert/update permission if it is insertable/updatable as returned by pg
      if (
        this.state.viewInfo &&
        'is_insertable_into' in this.state.viewInfo &&
        this.state.viewInfo.is_insertable_into === 'YES'
      ) {
        qTypes.push('insert');
      }

      qTypes.push('select');

      if (
        this.state.viewInfo &&
        'is_updatable' in this.state.viewInfo &&
        this.state.viewInfo.is_updatable === 'YES'
      ) {
        qTypes.push('update');
        qTypes.push('delete');
      }
    }

    const tSchema = allSchemas.find(t => t.table_name === tableName);

    const allRolesList = getAllRoles(allSchemas);

    return (
      <div className={styles.container}>
        {getHeader(tSchema)}
        <br />
        <div className={styles.padd_left_remove}>
          <div className={`${styles.padd_remove} col-xs-12`}>
            <h4 className={styles.subheading_text}>Permissions</h4>
            {getPermissionsTable(tSchema, qTypes, permissionsState, allRolesList)}
            {getBulkSection(tSchema, qTypes, permissionsState)}
            {getEditSection(tSchema, qTypes, permissionsState, allRolesList)}
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
  migrationMode: state.main.migrationMode,
  currentSchema: state.tables.currentSchema,
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
  ...state.tables.modify,
});

const permissionsConnector = connect => connect(mapStateToProps)(Permissions);

export default permissionsConnector;
