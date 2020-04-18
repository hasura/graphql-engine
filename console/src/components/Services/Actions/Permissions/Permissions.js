import React from 'react';
import { getActionPermissions, findActionPermission } from '../utils';
import Helmet from 'react-helmet';

import { fetchRoleList } from '../../Data/DataActions';
import PermTableHeader from '../../../Common/Permissions/TableHeader';
import PermTableBody from '../../../Common/Permissions/TableBody';
import { permissionsSymbols } from '../../../Common/Permissions/PermissionSymbols';
import { permOpenEdit, permCloseEdit, permSetRoleName } from './reducer';
import PermissionEditor from './PermissionEditor';
import { setDefaults } from './reducer';
import { Icon, Box } from '../../../UIKit/atoms';
import styles from '../../../Common/Permissions/PermissionStyles.scss';

const queryTypes = ['Permission'];

const Permissions = ({
  currentAction,
  allRoles,
  dispatch,
  permissionEdit,
  isEditing,
  isFetching,
}) => {
  React.useEffect(() => {
    dispatch(fetchRoleList());
    return () => {
      dispatch(setDefaults());
    };
  }, []);

  const allPermissions = getActionPermissions(currentAction);

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
      const dispatchRoleNameChange = e => {
        dispatch(permSetRoleName(e.target.value));
      };

      const getEditIcon = () => {
        return (
          <span className={styles.editPermsIcon}>
            <Icon type="edit" />
          </span>
        );
      };

      // get root types for a given role
      const getQueryTypes = (role, isNewRole) => {
        return queryTypes.map(queryType => {
          const dispatchOpenEdit = () => () => {
            if (isNewRole && !!role) {
              dispatch(permOpenEdit(role, isNewRole, true));
            } else if (role) {
              const existingPerm = findActionPermission(allPermissions, role);
              dispatch(permOpenEdit(role, isNewRole, !existingPerm));
            } else {
              document.getElementById('new-role-input').focus();
            }
          };

          const dispatchCloseEdit = () => {
            dispatch(permCloseEdit());
          };

          const isEditAllowed = role !== 'admin';
          const isCurrEdit =
            isEditing &&
            (permissionEdit.role === role ||
              (permissionEdit.isNewRole && permissionEdit.newRole === role));
          let editIcon = '';
          let className = '';
          // eslint-disable-next-line @typescript-eslint/no-empty-function
          let onClick = () => {};
          if (isEditAllowed) {
            editIcon = getEditIcon();

            className += styles.clickableCell;
            onClick = dispatchOpenEdit(queryType);
            if (isCurrEdit) {
              onClick = dispatchCloseEdit;
              className += ` ${styles.currEdit}`;
            }
          }

          const getRoleQueryPermission = () => {
            let _permission;
            if (role === 'admin') {
              _permission = permissionsSymbols.fullAccess;
            } else if (isNewRole) {
              _permission = permissionsSymbols.noAccess;
            } else {
              const existingPerm = findActionPermission(allPermissions, role);
              if (!existingPerm) {
                _permission = permissionsSymbols.noAccess;
              } else {
                _permission = permissionsSymbols.fullAccess;
              }
            }
            return _permission;
          };

          return {
            permType: queryType,
            className,
            editIcon: editIcon,
            onClick,
            dataTest: `${role}-${queryType}`,
            access: getRoleQueryPermission(),
          };
        });
      };

      // form rolesList and permissions metadata associated with each role
      const _roleList = ['admin', ...allRoles];
      const rolePermissions = _roleList.map(r => {
        return {
          roleName: r,
          permTypes: getQueryTypes(r, false),
        };
      });

      // push permissions metadata associated with the new role
      rolePermissions.push({
        roleName: permissionEdit.newRole,
        permTypes: getQueryTypes(permissionEdit.newRole, true),
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

  return (
    <div>
      <Helmet
        title={`Permissions - ${currentAction.action_name} - Actions | Hasura`}
      />
      {getPermissionsTable()}
      <Box mb="20px">
        <PermissionEditor
          permissionEdit={permissionEdit}
          dispatch={dispatch}
          isFetching={isFetching}
          isEditing={isEditing}
        />
      </Box>
    </div>
  );
};

export default Permissions;
