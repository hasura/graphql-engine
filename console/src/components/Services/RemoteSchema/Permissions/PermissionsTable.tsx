import React, { ChangeEvent } from 'react';
import { GraphQLSchema } from 'graphql';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import PermTableHeader from '../../../Common/Permissions/TableHeader';
import PermTableBody from '../../../Common/Permissions/TableBody';
import { permissionsSymbols } from '../../../Common/Permissions/PermissionSymbols';
import {
  buildSchemaFromRoleDefn,
  findRemoteSchemaPermission,
  getRemoteSchemaFields,
} from './utils';
import {
  RolePermissions,
  PermOpenEditType,
  PermissionsType,
  PermissionEdit,
} from './types';

export type PermissionsTableProps = {
  setSchemaDefinition: (data: string) => void;
  permOpenEdit: PermOpenEditType;
  permCloseEdit: () => void;
  permSetBulkSelect: (checked: boolean, role: string) => void;
  permSetRoleName: (name: string) => void;
  allRoles: string[];
  currentRemoteSchema: {
    name: string;
    permissions?: PermissionsType[];
  };
  schema: GraphQLSchema;
  bulkSelect: string[];
  readOnlyMode: boolean;
  permissionEdit: PermissionEdit;
  isEditing: boolean;
};

const queryTypes = ['Permission'];

const PermissionsTable: React.FC<PermissionsTableProps> = ({
  allRoles,
  currentRemoteSchema,
  permissionEdit,
  isEditing,
  bulkSelect,
  readOnlyMode,
  schema,
  permSetRoleName,
  permSetBulkSelect,
  setSchemaDefinition,
  permOpenEdit,
  permCloseEdit,
}) => {
  const allPermissions = currentRemoteSchema?.permissions || [];

  const headings = ['Role', ...queryTypes];

  const dispatchRoleNameChange = (e: ChangeEvent<HTMLInputElement>) => {
    permSetRoleName(e.target.value?.trim());
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
      permSetBulkSelect(isChecked, selectedRole as string);
    };

    const disableCheckbox = !findRemoteSchemaPermission(allPermissions, role);

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
      checked: bulkSelect.find((e: any) => e === role),
    };
  };

  const getQueryTypes = (role: string, isNewRole: boolean) => {
    return queryTypes.map(queryType => {
      const dispatchOpenEdit = () => () => {
        if (isNewRole && !!role) {
          setSchemaDefinition('');
          permOpenEdit(role, isNewRole, true);
        } else if (role) {
          const existingPerm = findRemoteSchemaPermission(allPermissions, role);
          permOpenEdit(role, isNewRole, !existingPerm);

          if (existingPerm) {
            const schemaDefinitionSdl = existingPerm.definition.schema;
            setSchemaDefinition(schemaDefinitionSdl);
          } else {
            setSchemaDefinition('');
          }
        } else {
          const inputFocusElem = document.getElementById('new-role-input');
          if (inputFocusElem) {
            inputFocusElem.focus();
          }
        }
      };

      const dispatchCloseEdit = () => {
        permCloseEdit();
        setSchemaDefinition('');
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
          const existingPerm = findRemoteSchemaPermission(allPermissions, role);
          if (existingPerm) {
            const remoteFields = getRemoteSchemaFields(
              schema,
              buildSchemaFromRoleDefn(existingPerm?.definition.schema)
            );
            permissionAccess = permissionsSymbols.fullAccess;

            if (
              remoteFields
                .filter(
                  field =>
                    !field.name.startsWith('enum') &&
                    !field.name.startsWith('scalar')
                )
                .some(field =>
                  field.children?.some(element => element.checked === false)
                )
            ) {
              permissionAccess = permissionsSymbols.partialAccess;
            }
          } else {
            permissionAccess = permissionsSymbols.noAccess;
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

  return (
    <div>
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
      <table className={`table table-bordered ${styles.permissionsTable}`}>
        <PermTableHeader headings={headings} />
        <PermTableBody
          rolePermissions={rolePermissions}
          dispatchRoleNameChange={dispatchRoleNameChange}
        />
      </table>
    </div>
  );
};

export default PermissionsTable;
