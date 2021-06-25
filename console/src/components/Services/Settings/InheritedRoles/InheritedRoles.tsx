import React, { useState } from 'react';
import { connect, ConnectedProps } from 'react-redux';
import {
  getInheritedRoles,
  rolesSelector,
} from '../../../../metadata/selector';
import { Dispatch, ReduxState } from '../../../../types';
import styles from '../Settings.scss';
import InheritedRolesTable, {
  InheritedRolesTableProps,
} from './InheritedRolesTable';

import { RoleActionsInterface } from './types';
import InheritedRolesEditor, { EditorProps } from './InheritedRolesEditor';
import { InheritedRole } from '../../../../metadata/types';
import { Badge, Heading } from '../../../UIKit/atoms';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  deleteInheritedRoleAction,
  addInheritedRoleAction,
  updateInheritedRoleAction,
} from '../../../../metadata/actions';

export const ActionContext = React.createContext<RoleActionsInterface | null>(
  null
);

const InheritedRoles: React.FC<Props> = props => {
  const { allRoles, inheritedRoles, experimentalFeatures, dispatch } = props;
  const [inheritedRoleName, setInheritedRoleName] = useState('');
  const [inheritedRole, setInheritedRole] = useState<InheritedRole | null>(
    null
  );
  const [isCollapsed, setIsCollapsed] = useState(true);

  const setEditorState = (
    roleName: string = inheritedRoleName,
    role: InheritedRole | null = inheritedRole,
    collapsed: boolean = isCollapsed
  ) => {
    setIsCollapsed(collapsed);
    setInheritedRole(role);
    setInheritedRoleName(roleName);
  };

  const onAdd = (roleName: string) => {
    setEditorState(roleName, null, false);
  };

  const onRoleNameChange = (roleName: string) => {
    setEditorState(roleName, null);
  };

  const onEdit = (role: InheritedRole) => {
    setEditorState('', role, false);
  };

  const resetState = () => {
    setEditorState('', null, true);
  };

  const onDelete = (role: InheritedRole) => {
    const confirmMessage = `This will delete the inherited role "${role?.role_name}"`;
    const isOk = getConfirmation(confirmMessage);
    if (isOk) {
      dispatch(deleteInheritedRoleAction(role?.role_name));
    }
  };

  const onSave = (role: InheritedRole) => {
    if (inheritedRole) {
      dispatch(updateInheritedRoleAction(role.role_name, role.role_set));
    } else {
      dispatch(addInheritedRoleAction(role.role_name, role.role_set));
    }
    resetState();
  };

  return (
    <div
      className={`${styles.clear_fix} ${styles.padd_left} ${styles.padd_top} ${styles.metadata_wrapper} container-fluid`}
    >
      {experimentalFeatures &&
      experimentalFeatures.includes('inherited_roles') ? (
        <>
          <div className={styles.header}>
            <Heading fontSize="24px">Inherited Roles</Heading>
            <span className={styles.headerBadge}>
              <Badge type="experimental" />
            </span>
          </div>
          <div className={styles.add_mar_top}>
            Inherited roles will combine the permissions of 2 or more roles.
          </div>
          <ActionContext.Provider
            value={{ onEdit, onAdd, onDelete, onRoleNameChange }}
          >
            <InheritedRolesTable inheritedRoles={inheritedRoles} />
          </ActionContext.Provider>
          <InheritedRolesEditor
            allRoles={allRoles}
            cancelCb={resetState}
            isCollapsed={isCollapsed}
            onSave={onSave}
            inheritedRoleName={inheritedRoleName}
            inheritedRole={inheritedRole}
          />
        </>
      ) : (
        <div>
          Inherited roles is currently an experimental feature. To enable
          inherited roles, start the Hasura server with environment variable
          <code>
            HASURA_GRAPHQL_EXPERIMENTAL_FEATURES: &quot;inherited_roles&quot;
          </code>
        </div>
      )}
    </div>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    inheritedRoles: getInheritedRoles(state),
    allRoles: rolesSelector(state),
    experimentalFeatures: state.main.serverConfig.data.experimental_features,
  };
};

const mapDispatchToProps = (dispatch: Dispatch) => {
  return {
    dispatch,
  };
};

const connector = connect(mapStateToProps, mapDispatchToProps);

type InjectedProps = ConnectedProps<typeof connector>;
type ComponentProps = InheritedRolesTableProps & EditorProps;

type Props = ComponentProps & InjectedProps;

const connectedInheritedRoles = connector(InheritedRoles);

export default connectedInheritedRoles;
