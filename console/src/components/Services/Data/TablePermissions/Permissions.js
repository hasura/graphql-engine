import PropTypes from 'prop-types';
import React, { Component } from 'react';
import JSONEditor from './JSONEditor';
import Tooltip from 'react-bootstrap/lib/Tooltip';
import InputGroup from 'react-bootstrap/lib/InputGroup';
import OverlayTrigger from 'react-bootstrap/es/OverlayTrigger';
import 'brace/mode/json';
import 'brace/theme/github';

import { RESET } from '../TableModify/ModifyActions';
import {
  permOpenEdit,
  permSetFilter,
  permSetFilterSameAs,
  permToggleField,
  permToggleAllFields,
  permAllowAll,
  permCloseEdit,
  permSetRoleName,
  // permToggleAllowUpsert,
  permToggleAllowAggregation,
  permToggleModifyLimit,
  permCustomChecked,
  // permRemoveRole,
  permSetBulkSelect,
  permRemoveMultipleRoles,
  permSetApplySamePerm,
  permDelApplySamePerm,
  permToggleBackendOnly,
  applySamePermissionsBulk,
  isQueryTypeBackendOnlyCompatible,
  SET_PRESET_VALUE,
  DELETE_PRESET,
  X_HASURA_CONST,
} from './Actions';

import PermTableHeader from '../../../Common/Permissions/TableHeader';
import PermTableBody from '../../../Common/Permissions/TableBody';
import { permissionsSymbols } from '../../../Common/Permissions/PermissionSymbols';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

import PermissionBuilder from './PermissionBuilder/PermissionBuilder';
import TableHeader from '../TableCommon/TableHeader';
import CollapsibleToggle from '../../../Common/CollapsibleToggle/CollapsibleToggle';
import Toggle from '../../../Common/Toggle/Toggle';
import EnhancedInput from '../../../Common/InputChecker/InputChecker';
import { fetchFunctionInit, setTable, updateSchemaInfo } from '../DataActions';
import { getIngForm, getEdForm } from '../utils';
import {
  getPermissionFilterString,
  getPermissionColumnAccessSummary,
  getTablePermissionsByRoles,
  getPermissionRowAccessSummary,
} from '../PermissionsSummary/utils';
import Button from '../../../Common/Button/Button';

import { NotFoundError } from '../../../Error/PageNotFound';
import {
  arrayDiff,
  deleteArrayElementAtIndex,
  getConfirmation,
  isEmpty,
  isJsonString,
  isObject,
  exists,
} from '../../../Common/utils/jsUtils';
import {
  findTable,
  generateTableDef,
  QUERY_TYPES,
  dataSource,
  isFeatureSupported,
} from '../../../../dataSources';
import KnowMoreLink from '../../../Common/KnowMoreLink/KnowMoreLink';
import {
  getFilterQueries,
  replaceLegacyOperators,
  updateFilterTypeLabel,
  getDefaultFilterType,
  getUpdateTooltip,
  getQuerySingleRowMutation,
  getPermissionsIcon,
} from './utils';
import PermButtonSection from './PermButtonsSection';
import { rolesSelector } from '../../../../metadata/selector';
import { RightContainer } from '../../../Common/Layout/RightContainer';
import FeatureDisabled from '../FeatureDisabled';

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
      localFilterString: {
        filter: '',
        check: '',
      },
      prevPermissionsState: {},
      presetsOrdered: [],
    };
  }

  componentDidMount() {
    const { dispatch } = this.props;

    if (!isFeatureSupported('tables.permissions.enabled')) return;

    dispatch({ type: RESET });
    dispatch(setTable(this.props.tableName));
    dispatch(fetchFunctionInit());

    if (this.props.permissionsState.inconsistentInhertiedRole) {
      const tableSchema = findTable(
        this.props.allSchemas,
        generateTableDef(
          this.props.permissionsState.inconsistentInhertiedRole.table,
          this.props.permissionsState.inconsistentInhertiedRole.schema
        )
      );
      dispatch(
        permOpenEdit(
          tableSchema,
          this.props.permissionsState.inconsistentInhertiedRole.role,
          this.props.permissionsState.inconsistentInhertiedRole.permission_type
        )
      );
    }
  }

  static getDerivedStateFromProps(nextProps, prevState) {
    const prevPermissionsState = prevState.prevPermissionsState;
    const nextPermissionsState = nextProps.permissionsState;

    const newState = {
      prevPermissionsState: nextPermissionsState,
    };

    if (
      prevPermissionsState.role !== nextPermissionsState.role ||
      prevPermissionsState.query !== nextPermissionsState.query
    ) {
      newState.localFilterString = {
        check: '',
        filter: '',
      };

      if (
        nextPermissionsState.query &&
        nextPermissionsState[nextPermissionsState.query] &&
        isObject(nextPermissionsState[nextPermissionsState.query].set)
      ) {
        newState.presetsOrdered = Object.keys(
          nextPermissionsState[nextPermissionsState.query].set
        );
      }
    }

    return newState;
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
      // tableType,
      allSchemas,
      schemaList,
      ongoingRequest,
      lastError,
      lastFormError,
      lastSuccess,
      permissionsState,
      migrationMode,
      readOnlyMode,
      currentSchema,
      allRoles,
      nonTrackableFunctions,
      trackableFunctions,
      currentSource,
    } = this.props;

    const { localFilterString, presetsOrdered } = this.state;

    const currentTableSchema = findTable(
      allSchemas,
      generateTableDef(tableName, currentSchema)
    );

    if (!isFeatureSupported('tables.permissions.enabled')) {
      return (
        <FeatureDisabled
          tab="permissions"
          tableName={tableName}
          schemaName={currentSchema}
        />
      );
    }

    if (
      !currentTableSchema &&
      isFeatureSupported('tables.permissions.enabled')
    ) {
      // throw a 404 exception
      throw new NotFoundError();
    }

    const allFunctions = nonTrackableFunctions.concat(trackableFunctions);
    const groupedComputedFields = dataSource.getGroupedTableComputedFields(
      currentTableSchema,
      allFunctions
    );

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
          source={currentSource}
          tabName="permissions"
          migrationMode={migrationMode}
          readOnlyMode={readOnlyMode}
        />
      );
    };

    const getPermissionsTable = (
      tableSchema,
      supportedQueryTypes,
      roleList
    ) => {
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
        if (!dataSource.viewsSupported) {
          return null;
        }

        let note;

        const unsupportedQueryTypes = arrayDiff(
          QUERY_TYPES,
          supportedQueryTypes
        );

        if (unsupportedQueryTypes.length) {
          note = (
            <div className={styles.permissionsLegend}>
              <i className="fa fa-info-circle" aria-hidden="true" />
              &nbsp; You cannot {unsupportedQueryTypes.join('/')} into this view
            </div>
          );
        }

        return note;
      };

      const getPermissionsTableHead = () => {
        const headings = ['Role', ...supportedQueryTypes, ''];
        return <PermTableHeader headings={headings} />;
      };

      const getPermissionsTableBody = () => {
        const rolePermissions = getTablePermissionsByRoles(tableSchema);

        const getBulkCheckbox = (role, isNewRole) => {
          const dispatchBulkSelect = e => {
            const isChecked = e.target.checked;
            const selectedRole = e.target.getAttribute('data-role');
            dispatch(permSetBulkSelect(isChecked, selectedRole));
          };

          const disableCheckbox = !Object.keys(rolePermissions).includes(role);

          return {
            showCheckbox: !(role === 'admin' || isNewRole),
            disableCheckbox,
            title: disableCheckbox
              ? 'No permissions exist'
              : 'Select for bulk actions',
            bulkSelect: permissionsState.bulkSelect,
            onChange: dispatchBulkSelect,
            role,
            isNewRole,
            checked: permissionsState.bulkSelect.filter(e => e === role).length,
          };
        };

        const getQueryTypes = role => {
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

          const getRoleQueryPermission = queryType =>
            permissionsSymbols[
              getPermissionsIcon(
                role,
                rolePermissions,
                queryType,
                tableSchema.columns,
                groupedComputedFields
              )
            ];
          return supportedQueryTypes.map(queryType => {
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
            return {
              className,
              permType: queryType,
              onClick,
              dataTest: `${role}-${queryType}`,
              access: getRoleQueryPermission(queryType),
              editIcon,
            };
          });
        };

        // add admin to roles
        const _roleList = ['admin'].concat(roleList);

        const _rolePermissions = _roleList.map(r => {
          return {
            roleName: r,
            permTypes: getQueryTypes(r),
            bulkSection: getBulkCheckbox(r, false),
          };
        });

        _rolePermissions.push({
          roleName: permissionsState.newRole,
          permTypes: getQueryTypes(permissionsState.newRole, true),
          bulkSection: getBulkCheckbox(permissionsState.newRole, true),
          isNewRole: true,
        });

        const dispatchRoleNameChange = e => {
          const newRole = e.target.value;
          if (permissionsState.query) {
            dispatch(
              permOpenEdit(tableSchema, newRole, permissionsState.query)
            );
          }
          dispatch(permSetRoleName(newRole));
        };

        return (
          <PermTableBody
            rolePermissions={_rolePermissions}
            dispatchRoleNameChange={dispatchRoleNameChange}
          />
        );
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

    const getEditSection = (tableSchema, supportedQueryTypes, roleList) => {
      if (!permissionsState.isEditing) {
        return;
      }

      const dispatchCloseEdit = () => {
        dispatch(permCloseEdit());
      };

      const query = permissionsState.query;

      const rolePermissions = tableSchema.permissions.find(
        p => p.role_name === permissionsState.role
      );

      const currQueryPermissions = rolePermissions
        ? rolePermissions.permissions[permissionsState.query]
        : undefined;

      const newQueryPermissions = permissionsState[query];

      const noPermissions = !newQueryPermissions;

      const noPermissionsMsg = 'Set row permissions first';

      const noFilterPermissionMsg = 'Set pre-update permissions first';

      const permsChanged =
        JSON.stringify(newQueryPermissions) !==
        JSON.stringify(currQueryPermissions);

      let sectionClasses = styles.editPermsSection;
      if (noPermissions) {
        sectionClasses += ' ' + styles.disabled;
      }

      const getSectionHeader = (title, toolTip, sectionStatus, knowMoreRef) => {
        let sectionStatusHtml;
        if (sectionStatus) {
          sectionStatusHtml = (
            <span className={styles.add_mar_left}>
              - <i className={styles.sectionStatus}>{sectionStatus}</i>
            </span>
          );
        }

        let knowMoreHtml;
        if (knowMoreRef) {
          knowMoreHtml = (
            <span
              className={`${styles.add_mar_left_small} ${styles.sectionStatus}`}
            >
              <KnowMoreLink href={knowMoreRef} />
            </span>
          );
        }

        return (
          <div>
            {addTooltip(title, toolTip)} {knowMoreHtml} {sectionStatusHtml}
          </div>
        );
      };

      const getRowSection = () => {
        let filterString;
        if (query === 'update') {
          filterString = {
            check: getPermissionFilterString(
              permissionsState[query],
              query,
              false,
              'check'
            ),
            filter: getPermissionFilterString(
              permissionsState[query],
              query,
              false,
              'filter'
            ),
          };
        } else {
          const key = getDefaultFilterType(query);
          filterString = {
            [key]: getPermissionFilterString(
              permissionsState[query],
              query,
              false,
              key
            ),
          };
        }

        const rowSectionStatus = getPermissionRowAccessSummary(
          filterString[getDefaultFilterType(query)]
        );

        filterString = replaceLegacyOperators(filterString);

        const getFilterOptions = (filterType, disabled = false) => {
          const currentFilterString = this.state.localFilterString[filterType];

          const dispatchAllowAll = () => {
            dispatch(permAllowAll(filterType));
          };

          const dispatchFuncSetFilter = filter => {
            this.setState(prev => ({
              localFilterString: {
                ...prev.localFilterString,
                [filterType]: filter,
              },
            }));

            if (isJsonString(filter)) {
              dispatch(permSetFilter(JSON.parse(filter), filterType));
            }
          };

          const dispatchSetFilterSameAs = filter => () => {
            dispatch(permSetFilterSameAs(JSON.parse(filter), filterType));
          };

          const dispatchCustomChecked = () => {
            dispatch(permCustomChecked(filterType));
          };

          const _filterOptionsSection = [];

          const filterQueries = getFilterQueries(
            supportedQueryTypes,
            permissionsState,
            filterType
          );

          const selectedValue = (
            <JSONEditor
              data={filterString[filterType] || currentFilterString}
              onChange={dispatchFuncSetFilter}
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
                  disabled={disabled}
                  title={disabled ? noFilterPermissionMsg : ''}
                  className={`legacy-input-fix ${styles.bottom5}`}
                  readOnly
                />
                {label}
              </label>
            </div>
          );

          // TODO: add no access option

          const addNoChecksOption = () => {
            const isSelected =
              !permissionsState.custom_checked[filterType] &&
              getPermissionRowAccessSummary(filterString[filterType]) ===
                'without any checks';

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
                !permissionsState.custom_checked[filterType] &&
                filterString[filterType] === filter;

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
            const loadSchemasFunc = schemaNames => {
              dispatch(updateSchemaInfo({ schemas: schemaNames }));
            };

            const isUniqueFilter =
              filterString[filterType] !== '' &&
              filterString[filterType] !== '{}' &&
              !filterQueries[filterString[filterType]];

            const isSelected =
              permissionsState.custom_checked[filterType] || isUniqueFilter;

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
                  allFunctions={allFunctions}
                  schemaList={schemaList}
                  filter={filterString[filterType]}
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
              permissionsState.select && exists(permissionsState.select.limit)
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

        const getUpdateFilterOptions = (filterType, disabled = false) => {
          return (
            <div
              className={disabled ? styles.disabled : ''}
              title={disabled ? noFilterPermissionMsg : ''}
            >
              <hr className="my-md" />
              {addTooltip(
                updateFilterTypeLabel[filterType],
                getUpdateTooltip(filterType)
              )}
              {getFilterOptions(filterType, disabled)}
            </div>
          );
        };

        const rowSectionTitle = 'Row ' + query + ' permissions';
        const singleRowMutation = getQuerySingleRowMutation(query);

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
                {permissionsState.query === 'update' ? (
                  <>
                    {getUpdateFilterOptions('filter')}
                    {getUpdateFilterOptions('check', noPermissions)}
                  </>
                ) : (
                  getFilterOptions(getDefaultFilterType(query))
                )}
              </div>
              <div className={styles.add_mar_top}>{getLimitSection()}</div>
              <div className={styles.add_mar_top}>
                {singleRowMutation && (
                  <span>
                    The single row mutation <b>{singleRowMutation}</b> shares
                    the returning type with the query field. Hence if no{' '}
                    <b>select</b> permissions are defined, the{' '}
                    <b>{singleRowMutation}</b> field will also be omitted from
                    the GraphQL schema.
                  </span>
                )}
              </div>
            </div>
          </CollapsibleToggle>
        );
      };

      const getColumnSection = () => {
        const getColumnList = () => {
          const _columnList = [];

          const dispatchToggleField = fieldType => e => {
            const fieldName = e.target.value;
            dispatch(permToggleField(fieldType, fieldName));
          };

          const getFieldCheckbox = (fieldType, fieldName) => {
            let checked = false;
            if (permissionsState[query]) {
              const permittedFields = permissionsState[query][fieldType] || [];

              if (permittedFields === '*') {
                checked = true;
              } else {
                checked = permittedFields.includes(fieldName);
              }
            }

            return (
              <div key={fieldName} className={styles.columnListElement}>
                <div className="checkbox">
                  <label>
                    <input
                      type="checkbox"
                      className="legacy-input-fix"
                      checked={checked}
                      value={fieldName}
                      onChange={dispatchToggleField(fieldType)}
                      disabled={noPermissions}
                      title={noPermissions ? noPermissionsMsg : ''}
                    />
                    {fieldType === 'columns' ? fieldName : <i>{fieldName}</i>}
                  </label>
                </div>
              </div>
            );
          };

          tableSchema.columns.forEach(colObj => {
            const columnName = colObj.column_name;

            _columnList.push(getFieldCheckbox('columns', columnName));
          });

          if (query === 'select') {
            groupedComputedFields.scalar.forEach(scalarComputedField => {
              const computedFieldName = scalarComputedField.computed_field_name;

              _columnList.push(
                getFieldCheckbox('computed_fields', computedFieldName)
              );
            });
          }

          _columnList.push(<div key={-1} className={styles.clear_fix} />);

          return _columnList;
        };

        const getExternalTablePermissionsMsg = () => {
          let _externalPermissionsMsg = '';

          // eg. relationships, table computed fields
          const externalObjects = [];

          if (tableSchema.relationships.length) {
            externalObjects.push('relationships');
          }

          if (query === 'select' && groupedComputedFields.table.length) {
            externalObjects.push('table computed fields');
          }

          if (externalObjects.length) {
            _externalPermissionsMsg = (
              <div className={styles.add_mar_top_small}>
                For <b>{externalObjects.join(', ')}</b>, set permissions for the
                corresponding tables/views.
              </div>
            );
          }

          return _externalPermissionsMsg;
        };

        const getToggleAllBtn = () => {
          const dispatchToggleAllColumns = () => {
            const allFields = {};

            allFields.columns = (tableSchema.columns || []).map(
              c => c.column_name
            );

            if (query === 'select') {
              allFields.computed_fields = groupedComputedFields.scalar.map(
                cf => cf.computed_field_name
              );
            }

            dispatch(permToggleAllFields(allFields));
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

          const tableFields = {};
          tableFields.columns = tableSchema.columns || [];
          if (query === 'select') {
            tableFields.computed_fields = groupedComputedFields.scalar;
          }

          const colSectionStatus = getPermissionColumnAccessSummary(
            permissionsState[query],
            tableFields
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

                {getExternalTablePermissionsMsg()}
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

        const presets = (queryState && queryState.set) || {};

        const getPresetValues = () => {
          const presetColumns = Object.keys(presets);

          const setPresetValue = e => {
            // Get the index of the changed value and if both key and value are set create one more object in set
            const inputNode = e.target;

            const column = inputNode.getAttribute('data-preset-column');

            const prefixVal =
              inputNode && inputNode.getAttribute('data-prefix-val');

            const actionData = {
              column,
              value: (prefixVal || '') + inputNode.value,
            };

            dispatch({
              type: SET_PRESET_VALUE,
              data: { ...actionData, queryType: query },
            });
          };

          const setPresetColumn = e => {
            // Get the index of the changed value and if both key and value are set create one more object in set
            const selectNode = e.target;
            const selectedColumn = selectNode.value;
            const selectedOption = selectNode.selectedOptions[0];

            const prevKey = selectNode.getAttribute('data-preset-column');
            const index = selectNode.getAttribute('data-index-id');

            const updatedPresetOrder = [...presetsOrdered];
            updatedPresetOrder[index] = selectedColumn;
            this.setState({ presetsOrdered: updatedPresetOrder });

            if (selectedOption) {
              const actionData = {};
              actionData.column = selectedColumn;
              actionData.prevKey = prevKey;

              dispatch({
                type: SET_PRESET_VALUE,
                data: { ...actionData, queryType: query },
              });
            }
          };

          const setPresetType = e => {
            const selectNode = e.target;

            const column = selectNode.getAttribute('data-preset-column');
            const actionData = {
              column,
              value: e.target.value === 'session' ? X_HASURA_CONST : '',
            };

            dispatch({
              type: SET_PRESET_VALUE,
              data: { ...actionData, queryType: query },
            });
          };

          const deletePreset = e => {
            const column = e.target.getAttribute('data-preset-column');
            const index = e.target.getAttribute('data-index-id');

            const updatedPresetOrder = [...presetsOrdered];
            deleteArrayElementAtIndex(updatedPresetOrder, index);
            this.setState({ presetsOrdered: updatedPresetOrder });

            dispatch({
              type: DELETE_PRESET,
              data: {
                column,
                queryType: query,
              },
            });
          };

          const getPresetValueType = preset => {
            let _valueType = '';

            if (preset.column || preset.value) {
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
                  const columnName = c.column_name;
                  if (
                    columnName === preset.column ||
                    !presetColumns.includes(columnName)
                  ) {
                    _columnOptions.push(
                      <option value={columnName} key={i}>
                        {columnName}
                      </option>
                    );
                  }
                });
              }

              return _columnOptions;
            };

            return (
              <select
                className="input-sm form-control"
                value={preset.column}
                onChange={setPresetColumn}
                data-preset-column={preset.column}
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

            const selectTypeDisabled = !preset.column;

            return (
              <select
                className="input-sm form-control"
                onChange={setPresetType}
                data-preset-column={preset.column}
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

            const presetInputDisabled = !preset.column;

            const columnInfo = columns.find(
              c => c.column_name === preset.column
            );
            const columnType = columnInfo
              ? dataSource.getColumnType(columnInfo)
              : '';

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
                    data-preset-column={preset.column}
                    data-prefix-val={X_HASURA_CONST}
                  />
                </InputGroup>
              );
            } else {
              _presetInput = (
                <EnhancedInput
                  placeholder="column_value"
                  type={columnType}
                  value={preset.value}
                  onChange={setPresetValue}
                  data-test={'column-presets-value-' + index}
                  data-preset-column={preset.column}
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
                  data-preset-column={preset.column}
                  data-index-id={index}
                />
              );
            }

            return _deleteBtn;
          };

          return presetsOrdered.concat('').map((presetColumn, i) => {
            const presetObj = {
              column: presetColumn,
              value: presets[presetColumn],
            };

            const rowElementStyle =
              styles.display_inline +
              ' ' +
              styles.add_mar_right +
              ' ' +
              styles.input_element_wrapper;

            return (
              <div
                className={styles.insertSetConfigRow}
                key={presetColumn || i}
              >
                <div className={rowElementStyle}>
                  {getPresetColumnSelect(presetObj, i)}
                </div>
                <div className={rowElementStyle}>
                  {getPresetTypeSelect(presetObj, i)}
                </div>
                <div className={rowElementStyle}>
                  {getPresetInput(presetObj, i)}
                </div>
                <div className={rowElementStyle}>
                  {getPresetExample(presetObj)}
                </div>
                <div className={rowElementStyle}>
                  {getDeleteButton(presetObj, i)}
                </div>
              </div>
            );
          });
        };

        const presetTooltip = (
          <Tooltip id="tooltip-insert-set-operations">
            Set static values or session variables as pre-determined values for
            columns while {getIngForm(query)}
          </Tooltip>
        );

        const presetStatus = !isEmpty(presets)
          ? Object.keys(presets).join(', ')
          : 'no presets';

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
        if (!isFeatureSupported('tables.permissions.aggregation')) {
          return null;
        }

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
                    className="legacy-input-fix"
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
        if (readOnlyMode) {
          return null;
        }

        // const applySameSelected = e => {
        //   const isChecked = e.target.checked;
        //   const selectedRole = e.target.getAttribute('data-role');
        //   dispatch(permSetSameSelect(isChecked, selectedRole));
        // };

        const applySameBulk = () => {
          const confirmMessage = 'This will overwrite any existing permissions';
          const isOk = getConfirmation(confirmMessage);
          if (isOk) {
            dispatch(applySamePermissionsBulk(tableSchema, permsChanged));
          }
        };

        const applyToList = permissionsState.applySamePermissions;

        const disabledCloneMsg = 'No permissions are set';

        const getApplyToList = () => {
          const _applyToListHtml = [];

          const tableOptions = allSchemas.map(schema => schema.table_name);
          const actionsList = supportedQueryTypes || [
            'insert',
            'select',
            'update',
            'delete',
          ];

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
              <hr className="my-md" />
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

      const getBackendOnlySection = () => {
        if (!isQueryTypeBackendOnlyCompatible(permissionsState.query)) {
          return null;
        }
        const tooltip = (
          <Tooltip id="tooltip-backend-only">
            When enabled, this {permissionsState.query} mutation is accessible
            only via "trusted backends"
          </Tooltip>
        );
        const isBackendOnly = !!(
          permissionsState[permissionsState.query] &&
          permissionsState[permissionsState.query].backend_only
        );
        const backendStatus = isBackendOnly ? 'enabled' : 'disabled';
        return (
          <CollapsibleToggle
            title={getSectionHeader(
              'Backend only',
              tooltip,
              backendStatus,
              'https://hasura.io/docs/latest/graphql/core/auth/authorization/permission-rules.html#backend-only'
            )}
            useDefaultTitleStyle
            testId={'toggle-backend-only'}
          >
            <div
              className={`${styles.editPermsSection} ${styles.display_flex}`}
            >
              <div
                className={`${styles.display_flex} ${styles.add_mar_right_mid}`}
              >
                <Toggle
                  checked={isBackendOnly}
                  onChange={() => dispatch(permToggleBackendOnly())}
                  icons={false}
                />
              </div>
              <span>Allow from backends only</span>
            </div>
          </CollapsibleToggle>
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
            {getPresetsSection('insert')}
            {getPresetsSection('update')}
            {getBackendOnlySection()}
            <PermButtonSection
              readOnlyMode={readOnlyMode}
              query={query}
              localFilterString={localFilterString}
              dispatch={dispatch}
              permissionsState={permissionsState}
              permsChanged={permsChanged}
              currQueryPermissions={currQueryPermissions}
            />
            {getClonePermsSection()}
          </div>
        </div>
      );
    };

    /********************/

    const supportedQueryTypes = dataSource.getTableSupportedQueries(
      currentTableSchema
    );

    return (
      <RightContainer>
        <div className={styles.container}>
          {getHeader(currentTableSchema)}
          <br />
          <div className={styles.padd_left_remove}>
            <div className={`${styles.padd_remove} col-xs-12`}>
              <h4 className={styles.subheading_text}>Permissions</h4>
              {getPermissionsTable(
                currentTableSchema,
                supportedQueryTypes,
                allRoles
              )}
              {getBulkSection(currentTableSchema)}
              {getEditSection(
                currentTableSchema,
                supportedQueryTypes,
                allRoles
              )}
            </div>
          </div>
          <div className={`${styles.fixed} hidden`}>
            {getAlertHtml(
              ongoingRequest,
              lastError,
              lastSuccess,
              lastFormError
            )}
          </div>
        </div>
      </RightContainer>
    );
  }
}

Permissions.propTypes = {
  dispatch: PropTypes.func.isRequired,
  tableName: PropTypes.string.isRequired,
  tableType: PropTypes.string.isRequired,
  allSchemas: PropTypes.array.isRequired,
  allRoles: PropTypes.array.isRequired,
  migrationMode: PropTypes.bool.isRequired,
  readOnlyMode: PropTypes.bool.isRequired,
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
  allRoles: rolesSelector(state),
  schemaList: state.tables.schemaList,
  migrationMode: state.main.migrationMode,
  nonTrackableFunctions: state.tables.nonTrackablePostgresFunctions || [],
  trackableFunctions: state.tables.postgresFunctions || [],
  readOnlyMode: state.main.readOnlyMode,
  currentSchema: state.tables.currentSchema,
  serverVersion: state.main.serverVersion ? state.main.serverVersion : '',
  currentSource: state.tables.currentDataSource,
  ...state.tables.modify,
});

const permissionsConnector = connect => connect(mapStateToProps)(Permissions);

export default permissionsConnector;
