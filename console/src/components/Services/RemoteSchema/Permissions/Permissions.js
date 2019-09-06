import React from 'react';
import { isObjectType } from 'graphql';
import { useIntrospectionSchema } from '../graphqlUtils';
import PermissionsEditor from './PermissionsEditor';
import PermTableHeader from '../../../Common/Permissions/TableHeader';
import PermTableBody from '../../../Common/Permissions/TableBody';
import Spinner from '../../../Common/Spinner/Spinner';
import styles from '../../../Common/Permissions/PermissionStyles.scss';
import { permissionsSymbols } from '../../../Common/Permissions/PermissionSymbols';
import {
  setPermissionRole,
  setCurrentPermissionEdit,
  closePermissionEdit,
} from './Actions';
import { parseRemoteRelPermDefinition, getRootTypeAccess } from './utils';
import { fetchRoleList } from '../../Data/DataActions';

const Permissions = props => {
  const {
    permissions: { editState, isFetching },
    remoteSchemaName,
    adminHeaders,
    remoteSchemasList,
    dispatch,
    rolesList,
    existingPermissions,
  } = props;

  const currentRemoteSchema = remoteSchemasList.find(
    r => r.name === remoteSchemaName
  );

  const { schema, loading, error, introspect } = useIntrospectionSchema(
    currentRemoteSchema.definition.url,
    adminHeaders,
    remoteSchemaName
  );

  React.useEffect(() => {
    dispatch(fetchRoleList());
  }, []);

  if (loading) return <Spinner />;

  if (error) {
    return (
      <div>
        <p>
          Error introspecting remote schema.{' '}
          <a onClick={introspect}>Try again.</a>
        </p>
      </div>
    );
  }

  const rootTypes = { query: '', mutation: '', subscription: '' };
  const queryTypeName = schema._queryType.name;
  rootTypes.query = queryTypeName;

  const mutationTypeName = schema._mutationType
    ? schema._mutationType.name
    : '';
  if (mutationTypeName) rootTypes.mutation = mutationTypeName;

  const subscriptionTypeName = schema._subscriptionType
    ? schema._subscriptionType.name
    : '';
  if (subscriptionTypeName) rootTypes.subscription = subscriptionTypeName;

  const objectTypes = {};
  const nonObjectTypes = {};
  objectTypes[queryTypeName] = schema._typeMap[queryTypeName];
  if (mutationTypeName) {
    objectTypes[mutationTypeName] = schema._typeMap[mutationTypeName];
  }
  Object.keys(schema._typeMap)
    .sort()
    .forEach(t => {
      if (
        t !== queryTypeName &&
        t !== mutationTypeName &&
        t.indexOf('__') !== 0
      ) {
        const currentType = schema._typeMap[t];
        if (isObjectType(currentType)) {
          objectTypes[t] = currentType;
        } else {
          nonObjectTypes[t] = currentType;
        }
      }
    });

  const getPermissionsTable = () => {
    const getPermissionsTableHead = () => {
      const headings = ['Role', ...Object.keys(rootTypes)];
      return <PermTableHeader headings={headings} />;
    };

    const getPermissionsTableBody = () => {
      const dispatchRoleNameChange = e => {
        dispatch(setPermissionRole(e.target.value));
      };

      const getEditIcon = () => {
        return (
          <span className={styles.editPermsIcon}>
            <i className="fa fa-pencil" aria-hidden="true" />
          </span>
        );
      };

      const getRootTypes = (role, isNewRole) => {
        return Object.keys(rootTypes).map(rootType => {
          const dispatchOpenEdit = rt => () => {
            if (isNewRole && !!role) {
              const perm = parseRemoteRelPermDefinition(
                null,
                rootTypes,
                objectTypes,
                nonObjectTypes,
                editState.role
              );
              dispatch(setCurrentPermissionEdit(perm, rt));
            } else if (role) {
              const perm = parseRemoteRelPermDefinition(
                existingPermissions.find(p => p.role === role),
                rootTypes,
                objectTypes,
                nonObjectTypes
              );
              dispatch(setCurrentPermissionEdit(perm, rt));
            } else {
              document.getElementById('new-role-input').focus();
            }
          };

          const dispatchCloseEdit = () => {
            dispatch(closePermissionEdit());
          };

          const isEditAllowed = role !== 'admin';
          const isCurrEdit =
            editState.editType === rootType &&
            (isNewRole
              ? editState.isNew && editState.newRole === role
              : editState.role === role);

          let editIcon = '';
          let className = '';
          let onClick = () => {};
          if (isEditAllowed) {
            editIcon = getEditIcon();

            className += styles.clickableCell;
            onClick = dispatchOpenEdit(rootType);
            if (isCurrEdit) {
              onClick = dispatchCloseEdit;
              className += ` ${styles.currEdit}`;
            }
          }

          const getRoleQueryPermission = rt => {
            let _permission;
            if (role === 'admin') {
              _permission = permissionsSymbols.fullAccess;
            } else if (isNewRole) {
              _permission = permissionsSymbols.noAccess;
            } else {
              const existingPerm = existingPermissions.find(
                p => p.role === role
              );
              if (!existingPerm) {
                _permission = permissionsSymbols.noAccess;
              } else {
                const perm = parseRemoteRelPermDefinition(
                  existingPerm,
                  rootTypes,
                  objectTypes,
                  nonObjectTypes
                );
                _permission =
                  permissionsSymbols[
                    getRootTypeAccess(rt, perm.allowedTypes, objectTypes)
                  ];
              }
            }
            return _permission;
          };

          return {
            permType: rootType,
            className,
            editIcon: editIcon,
            onClick,
            dataTest: `${role}-${rootType}`,
            access: getRoleQueryPermission(rootTypes[rootType]),
          };
        });
      };

      const uniqueRolesList = [
        'admin',
        ...rolesList,
        ...existingPermissions.map(ep => ep.role),
      ];
      const _roleList = [...new Set(uniqueRolesList)];

      const rolePermissions = _roleList.map(r => {
        return {
          roleName: r,
          permTypes: getRootTypes(r, false),
        };
      });

      rolePermissions.push({
        roleName: editState.newRole,
        permTypes: getRootTypes(editState.newRole, true),
        isNewRole: true,
      });

      return (
        <PermTableBody
          rolePermissions={rolePermissions}
          dispatchRoleNameChange={dispatchRoleNameChange}
        />
      );
    };

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
      {getPermissionsTable()}
      <div className={`${styles.add_mar_bottom}`}>
        <PermissionsEditor
          editState={editState}
          objectTypes={objectTypes}
          nonObjectTypes={nonObjectTypes}
          rootTypes={rootTypes}
          dispatch={props.dispatch}
          rolesList={rolesList}
          isFetching={isFetching}
        />
      </div>
    </div>
  );
};

export default Permissions;
