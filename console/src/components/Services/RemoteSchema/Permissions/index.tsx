import React from 'react';
import { connect, ConnectedProps } from 'react-redux';
import Permissions, { PermissionsProps } from './Permissions';
import RSPWrapper, { RSPWrapperProps } from './RSPWrapper';
import {
  permRemoveMultipleRoles,
  VIEW_REMOTE_SCHEMA,
  saveRemoteSchemaPermission,
  removeRemoteSchemaPermission,
} from '../Actions';
import {
  permCloseEdit,
  setSchemaDefinition,
  setDefaults,
  permOpenEdit,
  permSetRoleName,
  permSetBulkSelect,
} from './reducer';
import { Dispatch, ReduxState } from '../../../../types';
import {
  getRemoteSchemas,
  rolesSelector,
  getRemoteSchemaPermissions,
} from '../../../../metadata/selector';

export type RSPContainerProps = {
  rspEnabled: boolean;
};

const RSP: React.FC<Props> = props => {
  const { allRemoteSchemas, params, viewRemoteSchema, rspEnabled } = props;
  return (
    <RSPWrapper
      params={params}
      allRemoteSchemas={allRemoteSchemas}
      tabName="permissions"
      viewRemoteSchema={viewRemoteSchema}
      permissionRenderer={currentRemoteSchema =>
        rspEnabled ? (
          <Permissions {...props} {...{ currentRemoteSchema }} />
        ) : (
          <div>
            Remote schema permissions are not enabled. To enable remote schema
            permissions, start the Hasura server with environment variable
            <code>
              HASURA_GRAPHQL_ENABLE_REMOTE_SCHEMA_PERMISSIONS: &quot;true&quot;
            </code>
          </div>
        )
      }
    />
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    ...getRemoteSchemaPermissions(state),
    ...state.remoteSchemas,
    allRoles: rolesSelector(state),
    allRemoteSchemas: getRemoteSchemas(state),
    rspEnabled:
      state.main.serverConfig?.data?.is_remote_schema_permissions_enabled,
    readOnlyMode: state.main.readOnlyMode,
  };
};

const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    dispatch,
    permRemoveMultipleRoles: () => dispatch(permRemoveMultipleRoles()),
    viewRemoteSchema: (data: string) =>
      dispatch({ type: VIEW_REMOTE_SCHEMA, data }),
    saveRemoteSchemaPermission: (
      successCb?: () => void,
      errorCb?: () => void
    ) => dispatch(saveRemoteSchemaPermission(successCb, errorCb)),
    removeRemoteSchemaPermission: (
      successCb?: () => void,
      errorCb?: () => void
    ) => dispatch(removeRemoteSchemaPermission(successCb, errorCb)),
    setSchemaDefinition: (data: string) => dispatch(setSchemaDefinition(data)),
    setDefaults: () => dispatch(setDefaults()),
    permCloseEdit: () => dispatch(permCloseEdit()),
    permOpenEdit: (role: string, newRole: boolean, existingPerms: boolean) =>
      dispatch(permOpenEdit(role, newRole, existingPerms)),
    permSetBulkSelect: (checked: boolean, role: string) =>
      dispatch(permSetBulkSelect(checked, role)),
    permSetRoleName: (name: string) => dispatch(permSetRoleName(name)),
  };
};

const connector = connect(mapStateToProps, mapDispatchToProps);

type InjectedProps = ConnectedProps<typeof connector>;

type ComponentProps = RSPWrapperProps & PermissionsProps & RSPContainerProps;

type Props = ComponentProps & InjectedProps;

const RSPContainer = connector(RSP);
export default RSPContainer;
