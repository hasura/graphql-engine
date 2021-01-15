import React from 'react';
import { connect } from 'react-redux';
import Permissions, { PermissionsProps } from './Permissions';
import RSPWrapper from './RSPWrapper';
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
  allRoles: string[];
  allRemoteSchemas: { [key: string]: any }[];
  params: { [key: string]: string };
  viewRemoteSchema: (data: string) => void;
};

const RSP: React.FC<RSPContainerProps & PermissionsProps> = ({
  allRoles,
  allRemoteSchemas,
  ...props
}) => {
  const { params, viewRemoteSchema } = props;
  return (
    <RSPWrapper
      params={params}
      allRemoteSchemas={allRemoteSchemas}
      tabName="permissions"
      viewRemoteSchema={viewRemoteSchema}
    >
      <Permissions allRoles={allRoles} {...props} />
    </RSPWrapper>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    ...getRemoteSchemaPermissions(state),
    ...state.remoteSchemas,
    allRoles: rolesSelector(state),
    allRemoteSchemas: getRemoteSchemas(state),
    readOnlyMode: state.main.readOnlyMode,
  };
};

const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    dispatch,
    permRemoveMultipleRoles: () => dispatch(permRemoveMultipleRoles()),
    viewRemoteSchema: (data: string) =>
      dispatch({ type: VIEW_REMOTE_SCHEMA, data }),
    saveRemoteSchemaPermission: (data: any) =>
      dispatch(saveRemoteSchemaPermission(data)),
    removeRemoteSchemaPermission: (data: any) =>
      dispatch(removeRemoteSchemaPermission(data)),
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

const RSPContainer=connect(mapStateToProps, mapDispatchToProps)(RSP);
export default RSPContainer
