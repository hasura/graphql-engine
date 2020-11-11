import React from 'react';
import Permissions from './Permissions';
import RemoteSchemaContainer from '../Containers/remoteSchemaContainer';
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
import { fetchRoleList } from '../../Data/DataActions';
import { Dispatch } from '../../../../types';

// TODO : types
const PermWrapper = ({ allRoles, allRemoteSchemas, ...props }: any) => {
  console.log(props);
  const { params, viewRemoteSchema } = props;
  return (
    <RemoteSchemaContainer
      params={params}
      allRemoteSchemas={allRemoteSchemas}
      tabName="permissions"
      viewRemoteSchema={viewRemoteSchema}
    >
      <Permissions allRoles={allRoles} {...props} />
    </RemoteSchemaContainer>
  );
};

// TODO : types
const mapStateToProps = (state: any) => {
  return {
    ...state.remoteSchemas.permissions,
    allRoles: state.tables.allRoles,
    allRemoteSchemas: state.remoteSchemas.listData.remoteSchemas,
    readOnlyMode: state.main.readOnlyMode,
  };
};

// TODO : types
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
    setSchemaDefinition: (data: any) => dispatch(setSchemaDefinition(data)),
    fetchRoleList: () => dispatch(fetchRoleList()),
    setDefaults: () => dispatch(setDefaults()),
    permCloseEdit: () => dispatch(permCloseEdit()),
    permOpenEdit: (role: string, newRole: boolean, existingPerms: boolean) =>
      dispatch(permOpenEdit(role, newRole, existingPerms)),
    permSetBulkSelect: (checked: boolean, role: string) =>
      dispatch(permSetBulkSelect(checked, role)),
    permSetRoleName: (name: string) => dispatch(permSetRoleName(name)),
  };
};

export default (connect: any) =>
  connect(mapStateToProps, mapDispatchToProps)(PermWrapper);
