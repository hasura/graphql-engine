import React, { useEffect, useState } from 'react';
import { redirectToMetadataStatus } from '../../../Common/utils/routesUtils';
import { Analytics, REDACT_EVERYTHING } from '../../../../features/Analytics';
import { connect, ConnectedProps } from 'react-redux';
import { getInheritedRoles } from '../../../../metadata/selector';
import { Dispatch, ReduxState } from '../../../../types';
import InheritedRolesTable, {
  InheritedRolesTableProps,
} from './InheritedRolesTable';

import { RoleActionsInterface } from './types';
import InheritedRolesEditor, { EditorProps } from './InheritedRolesEditor';
import { InheritedRole } from '../../../../metadata/types';
import { Heading } from '../../../UIKit/atoms';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import {
  deleteInheritedRoleAction,
  addInheritedRoleAction,
  updateInheritedRoleAction,
} from '../../../../metadata/actions';
import {
  MetadataSelectors,
  useMetadata,
} from '../../../../features/hasura-metadata-api';
import Skeleton from 'react-loading-skeleton';

export const ActionContext = React.createContext<RoleActionsInterface | null>(
  null
);

let lastinconsistency: any[];

const InheritedRoles: React.FC<Props> = props => {
  const { data: allRoles = [], isLoading } = useMetadata(
    MetadataSelectors.getRoles
  );

  const { inheritedRoles, inconsistentInheritedRoles, dispatch } = props;
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

  useEffect(() => {
    if (
      inconsistentInheritedRoles &&
      inconsistentInheritedRoles?.length &&
      lastinconsistency !== inconsistentInheritedRoles
    ) {
      lastinconsistency = inconsistentInheritedRoles;
      // redirection will happen when there is an inconsitency detected for the first time
      // second time inherited roles page will load as expected
      // whenever there is an updation/ creation cause the inconsistency, it will redirect to status page once
      dispatch(redirectToMetadataStatus());
    }
  }, [inconsistentInheritedRoles, dispatch]);

  return (
    <Analytics name="InheritedRoles" {...REDACT_EVERYTHING}>
      <div className="clear-both pl-md pt-md mb-md bootstrap-jail">
        <div className="flex items-center">
          <Heading fontSize="24px">Inherited Roles</Heading>
        </div>
        <div className="mt-md">
          Inherited roles will combine the permissions of 2 or more roles.
        </div>
        <ActionContext.Provider
          value={{ onEdit, onAdd, onDelete, onRoleNameChange }}
        >
          <InheritedRolesTable inheritedRoles={inheritedRoles} />
        </ActionContext.Provider>

        {isLoading ? (
          <Skeleton count={8} height={20} />
        ) : (
          <InheritedRolesEditor
            allRoles={allRoles}
            cancelCb={resetState}
            isCollapsed={isCollapsed}
            onSave={onSave}
            inheritedRoleName={inheritedRoleName}
            inheritedRole={inheritedRole}
          />
        )}
      </div>
    </Analytics>
  );
};

const mapStateToProps = (state: ReduxState) => {
  return {
    inheritedRoles: getInheritedRoles(state),
    experimentalFeatures: state.main.serverConfig.data.experimental_features,
    inconsistentInheritedRoles: state.metadata.inconsistentInheritedRoles,
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
