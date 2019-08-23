import React from 'react';
import { isObjectType } from 'graphql';
import { useIntrospectionSchema } from '../graphqlUtils';
import PermissionsEditor from './PermissionsEditor';
import PermTableHeader from '../../../Common/Layout/Permissions/TableHeader';
import PermTableBody from '../../../Common/Layout/Permissions/TableBody';
import styles from '../../Data/TableModify/ModifyTable.scss';
import {
  setPermissionRole,
  setPermissionTypes,
  createRemoteSchemaPermission,
} from './Actions';
import { parseRemoteRelPermDefinition } from './utils';

const roleList = ['role1', 'role2', 'anonymous'];

const Permissions = props => {

  const {
    permissions: { rolePermissions },
    remoteSchemaName,
    adminHeaders,
    remoteSchemasList,
  } = props;

  const currentRemoteSchema = remoteSchemasList.find(
    r => r.name === remoteSchemaName
  );

  const { schema, loading, error, introspect } = useIntrospectionSchema(
    currentRemoteSchema.definition.url,
    adminHeaders
  );

  if (loading) return 'Loading...';

  if (error) {
    return (
      <div>
        <p>
          Error. <a onClick={introspect}>Retry </a>
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

  const permissionsSymbols = {
    fullAccess: (
      <i
        className={'fa fa-check ' + styles.permissionSymbolFA}
        aria-hidden="true"
      />
    ),
    noAccess: (
      <i
        className={'fa fa-times ' + styles.permissionSymbolNA}
        aria-hidden="true"
      />
    ),
    partialAccess: (
      <i
        className={'fa fa-filter ' + styles.permissionSymbolPA}
        aria-hidden="true"
      />
    ),
  };

  const getPermissionsTableHead = () => {
    const headings = ['Actions', 'Role', ...Object.keys(rootTypes)];
    return <PermTableHeader headings={headings} />;
  };

  const getPermissionsTableBody = () => {
    const dispatchRoleNameChange = e => {
      dispatch(permSetRoleName(e.target.value));
    };

    const getEditLink = () => {
      return (
        <span className={styles.editPermsLink}>
          <i className="fa fa-pencil" aria-hidden="true" />
        </span>
      );
    };

    const getRootTypes = (role, isNewRole) => {
      return Object.keys(rootTypes).map(rootType => {
        const dispatchOpenEdit = rt => () => {
          if (isNewRole && permissionsState.newRole !== '') {
            // TODO ON EDIT
          } else if (role !== '') {
            // TODO ON EDIT
          } else {
            document.getElementById('newRoleInput').focus();
          }
        };

        const dispatchCloseEdit = () => {
        };

        const isEditAllowed = role !== 'admin';
        const isCurrEdit = false;
          

        let editLink = '';
        let className = '';
        let onClick = () => {};
        if (isEditAllowed) {
          editLink = getEditLink();

          className += styles.clickableCell;
          onClick = dispatchOpenEdit(rootType);
          if (isCurrEdit) {
            onClick = dispatchCloseEdit;
            className += ` ${styles.currEdit}`;
          }
        }

        const getRoleQueryPermission = rt => {
          let _permission;

          const rolePermissions = {};

          if (role === 'admin') {
            _permission = permissionsSymbols.fullAccess;
          } else {
            _permission = permissionsSymbols.noAccess;
          }
          return _permission;
        };

        return {
          name: rootType,
          className,
          editLink,
          onClick,
          permSymbol: getRoleQueryPermission(rootType),
        };
      });
    }


    const _roleList = ['admin'].concat(roleList);

    // roles wrapper
    const roles = _roleList.map(role => {
      const _roleProps = {};
      _roleProps.name = role;
      _roleProps.dispatchBulkSelect = e => {
        const isChecked = e.target.checked;
        const selectedRole = e.target.getAttribute('data-role');
        //dispatch
      };
      _roleProps.bulkCheck = false
      return _roleProps;
    });
    return (
      <PermTableBody
        rolePermissions={roles}
        dispatchRoleNameChange={dispatchRoleNameChange}
        getPermTypes={getRootTypes}
      />
    )
  }

  return (
    <table className={`table table-bordered ${styles.permissionsTable}`}>
      {getPermissionsTableHead()}
      {getPermissionsTableBody()}
    </table>
  )

  const numPermissions = rolePermissions.length;
  return rolePermissions.map((rp, index) => {
    return (
      <PermissionsEditor
        permission={rp}
        objectTypes={objectTypes}
        nonObjectTypes={nonObjectTypes}
        key={index}
        index={index}
        numPermissions={numPermissions}
        isLast={index === numPermissions - 1}
        rootTypes={rootTypes}
        dispatch={props.dispatch}
      />
    );
  });
};

export default Permissions;
