import React, { ChangeEvent } from 'react';
import Helmet from 'react-helmet';
import { parse as sdlParse } from 'graphql';

import {
  getRemoteSchemaPermissions,
  findRemoteSchemaPermission,
} from '../utils';
import { fetchRoleList } from '../../Data/DataActions';
import PermTableHeader from '../../../Common/Permissions/TableHeader';
import PermTableBody from '../../../Common/Permissions/TableBody';
import { permissionsSymbols } from '../../../Common/Permissions/PermissionSymbols';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import PermissionEditor from './PermissionEditor';
import {
  permOpenEdit,
  permCloseEdit,
  permSetRoleName,
  setDefaults,
  permSetBulkSelect,
  setSchemaDefinition,
} from './reducer';
import { defaultSchemaDefSdl } from './state';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import { permRemoveMultipleRoles } from '../Actions';
import Button from '../../../Common/Button/Button';
import { Dispatch } from '../../../../types';

const queryTypes = ['Permission'];

export type PermissionsProps = {
  currentRemoteSchema: {
    name: string;
    permissions: Record<string, string>;
  };
  readOnlyMode: boolean;
  dispatch: Dispatch;
  isEditing: boolean;
  isFetching: boolean;
  allRoles: string[];
  bulkSelect: string[];
  permissionEdit: { isNewRole: boolean; role: string; newRole: string };
  schemaDefinition: any; // TODO: provide the right type here
};

export type RolePermissions = {
  roleName: string;
  permTypes: Record<string, any>;
  bulkSection: Record<string, any>;
  isNewRole?: boolean;
};

const Permissions = ({
  currentRemoteSchema,
  allRoles,
  dispatch,
  permissionEdit,
  isEditing,
  isFetching,
  bulkSelect,
  schemaDefinition,
  readOnlyMode = false,
}: PermissionsProps) => {
  React.useEffect(() => {
    dispatch(fetchRoleList());
    return () => {
      dispatch(setDefaults());
    };
  }, []);

  const allPermissions = getRemoteSchemaPermissions(currentRemoteSchema);

  const getPermissionsTable = () => {
    const getPermissionsLegend = () => (
      <div>
        <div className={styles.permissionsLegend}>
          <span className={styles.permissionsLegendValue}>
            {permissionsSymbols.fullAccess} : allowed
          </span>
          <span className={styles.permissionsLegendValue}>
            {permissionsSymbols.noAccess} : not allowed
          </span>
        </div>
      </div>
    );

    const getPermissionsTableHead = () => {
      const headings = ['Role', ...queryTypes];
      return <PermTableHeader headings={headings} />;
    };

    const getPermissionsTableBody = () => {
      const dispatchRoleNameChange = (e: ChangeEvent<HTMLInputElement>) => {
        dispatch(permSetRoleName(e.target.value));
      };

      const getEditIcon = () => {
        return (
          <span className={styles.editPermsIcon}>
            <i className="fa fa-pencil" aria-hidden="true" />
          </span>
        );
      };

      const getBulkCheckbox = (role: string, isNewRole: boolean) => {
        const dispatchBulkSelect = (e: ChangeEvent<HTMLInputElement>) => {
          const isChecked = e.target.checked;
          const selectedRole = e.target.getAttribute('data-role');
          dispatch(permSetBulkSelect(isChecked, selectedRole));
        };

        const disableCheckbox = !findRemoteSchemaPermission(
          allPermissions,
          role
        );

        return {
          showCheckbox: !(role === 'admin' || isNewRole),
          disableCheckbox,
          title: disableCheckbox
            ? 'No permissions exist'
            : 'Select for bulk actions',
          bulkSelect,
          onChange: dispatchBulkSelect,
          role,
          isNewRole,
          checked: bulkSelect.find(e => e === role),
        };
      };

      // get root types for a given role
      const getQueryTypes = (role: string, isNewRole: boolean) => {
        return queryTypes.map(queryType => {
          const dispatchOpenEdit = () => () => {
            if (isNewRole && !!role) {
              dispatch(permOpenEdit(role, isNewRole, true));
            } else if (role) {
              const existingPerm = findRemoteSchemaPermission(
                allPermissions,
                role
              );
              dispatch(permOpenEdit(role, isNewRole, !existingPerm));

              if (existingPerm) {
                const schemaDefinitionSdl = existingPerm.definition.schema;
                dispatch(
                  setSchemaDefinition(
                    schemaDefinitionSdl,
                    null,
                    null,
                    sdlParse(schemaDefinitionSdl)
                  )
                );
              } else {
                dispatch(
                  setSchemaDefinition(defaultSchemaDefSdl, null, null, null)
                );
              }
            } else {
              // FIXME: probably best if we handle the React way.
              const inputFocusElem = document.getElementById('new-role-input');
              if (inputFocusElem) {
                inputFocusElem.focus();
              }
            }
          };

          const dispatchCloseEdit = () => {
            dispatch(permCloseEdit());
            dispatch(
              setSchemaDefinition(defaultSchemaDefSdl, null, null, null)
            );
          };

          const isCurrEdit =
            isEditing &&
            (permissionEdit.role === role ||
              (permissionEdit.isNewRole && permissionEdit.newRole === role));
          let editIcon;
          let className = '';
          let onClick = () => {};
          if (role !== 'admin' && !readOnlyMode) {
            editIcon = getEditIcon();

            if (isCurrEdit) {
              onClick = dispatchCloseEdit;
              className += styles.currEdit;
            } else {
              className += styles.clickableCell;
              onClick = dispatchOpenEdit();
            }
          }

          const getRoleQueryPermission = () => {
            let permissionAccess;
            if (role === 'admin') {
              permissionAccess = permissionsSymbols.fullAccess;
            } else if (isNewRole) {
              permissionAccess = permissionsSymbols.noAccess;
            } else {
              const existingPerm = findRemoteSchemaPermission(
                allPermissions,
                role
              );
              if (!existingPerm) {
                permissionAccess = permissionsSymbols.noAccess;
              } else {
                permissionAccess = permissionsSymbols.fullAccess;
              }
            }
            return permissionAccess;
          };

          return {
            permType: queryType,
            className,
            editIcon,
            onClick,
            dataTest: `${role}-${queryType}`,
            access: getRoleQueryPermission(),
          };
        });
      };

      // form rolesList and permissions metadata associated with each role
      const roleList = ['admin', ...allRoles];
      const rolePermissions: RolePermissions[] = roleList.map(r => {
        return {
          roleName: r,
          permTypes: getQueryTypes(r, false),
          bulkSection: getBulkCheckbox(r, false),
        };
      });

      // push permissions metadata associated with the new role
      rolePermissions.push({
        roleName: permissionEdit.newRole,
        permTypes: getQueryTypes(permissionEdit.newRole, true),
        bulkSection: getBulkCheckbox(permissionEdit.newRole, true),
        isNewRole: true,
      });

      // form permissions table body based on the roles and their metadata
      return (
        <PermTableBody
          rolePermissions={rolePermissions}
          dispatchRoleNameChange={dispatchRoleNameChange}
        />
      );
    };

    return (
      <div>
        {getPermissionsLegend()}
        <table className={`table table-bordered ${styles.permissionsTable}`}>
          {getPermissionsTableHead()}
          {getPermissionsTableBody()}
        </table>
      </div>
    );
  };

  const getBulkSection = () => {
    if (!bulkSelect.length) {
      return;
    }

    const getSelectedRoles = () => {
      return bulkSelect.map(r => {
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
        // TODO
        dispatch(permRemoveMultipleRoles());
      }
    };

    return (
      <div id="bulk-section" className={styles.activeEdit}>
        <div className={styles.editPermsHeading}>Apply Bulk Actions</div>
        <div>
          <span className={styles.add_pad_right}>Selected Roles</span>
          {getSelectedRoles()}
        </div>
        <div className={`${styles.add_mar_top} ${styles.add_mar_bottom_mid}`}>
          <Button onClick={handleBulkRemoveClick} color="red" size="sm">
            Remove All Permissions
          </Button>
        </div>
      </div>
    );
  };

  return (
    <div>
      <Helmet
        title={`Permissions - ${currentRemoteSchema.name} - Remote Schemas | Hasura`}
      />
      {getPermissionsTable()}
      {getBulkSection()}
      <div className={`${styles.add_mar_bottom}`}>
        {!readOnlyMode && (
          <PermissionEditor
            permissionEdit={permissionEdit}
            dispatch={dispatch}
            isFetching={isFetching}
            isEditing={isEditing}
            readOnlyMode={readOnlyMode}
            schemaDefinition={schemaDefinition}
          />
        )}
      </div>
    </div>
  );
};

export default Permissions;
