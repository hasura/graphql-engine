import React, { useReducer } from 'react';
import { connect, ConnectedProps } from 'react-redux';

import {
  getFunctions,
  getTableInformation,
  rolesSelector,
} from '../../../../../../metadata/selector';
import { permissionsSymbols } from '../../../../../Common/Permissions/PermissionSymbols';
import PermTableBody from '../../../../../Common/Permissions/TableBody';
import PermTableHeader from '../../../../../Common/Permissions/TableHeader';
import { mapDispatchToPropsEmpty } from '../../../../../Common/utils/reactUtils';
import {
  dropFunctionPermission,
  setFunctionPermission,
} from '../../customFunctionReducer';
import PermissionEditor from './PermissionEditor';
import { ReduxState } from '../../../../../../types';
import {
  FunctionPermission,
  SelectPermissionEntry,
} from '../../../../../../metadata/types';
import Tooltip from '../../../../../Common/Tooltip/Tooltip';

import styles from '../../../../../Common/Permissions/PermissionStyles.scss';

const getFunctionPermissions = (
  allFunctions: InjectedProps['allFunctions'],
  currentFunctionSchema: string,
  currentFunctionName: string
) =>
  allFunctions.find(
    fn =>
      fn.function_name === currentFunctionName &&
      fn.function_schema === currentFunctionSchema
  )?.permissions;

const findFunctionPermissions = (
  allPermissions: FunctionPermission[] | undefined | null,
  userRole: string
) => {
  if (!allPermissions) {
    return undefined;
  }
  return allPermissions.find(permRole => permRole.role === userRole);
};

const getRoleQueryPermissionSymbol = (
  allPermissions: FunctionPermission[] | undefined | null,
  permissionRole: string,
  selectPermissionsForTable: SelectPermissionEntry[] | null,
  isEditable: boolean
) => {
  if (permissionRole === 'admin') {
    return permissionsSymbols.fullAccess;
  }

  let isTableSelectPermissionsEnabled = false;
  let isPermissionsEnabledOnMetadata = false;

  // Checking if select permissions are there on the reference table
  if (
    selectPermissionsForTable &&
    selectPermissionsForTable.find(
      selectPermissionEntry => selectPermissionEntry.role === permissionRole
    )
  ) {
    isTableSelectPermissionsEnabled = true;
  }

  // If permissions are inferred and not editable we only need to know if corresponding table has select permissions
  if (!isEditable) {
    return isTableSelectPermissionsEnabled
      ? permissionsSymbols.fullAccess
      : permissionsSymbols.noAccess;
  }

  // Checking if permissions are enabled and visible in the metadata
  if (findFunctionPermissions(allPermissions, permissionRole)) {
    isPermissionsEnabledOnMetadata = true;
  }

  if (!isPermissionsEnabledOnMetadata) {
    return permissionsSymbols.noAccess;
  }

  if (isPermissionsEnabledOnMetadata && !isTableSelectPermissionsEnabled) {
    return permissionsSymbols.partialAccessWarning;
  }

  return permissionsSymbols.fullAccess;
};

const initialState = {
  isEditing: false,
  role: '',
};
type ReducerState = typeof initialState;
type ReducerAction = { type: string; role: string };

const PERM_UPDATE_OPEN_STATE = 'PERM_UPDATE_OPEN_STATE';
const PERM_UPDATE_CLOSE_STATE = 'PERM_UPDATE_CLOSE_STATE';

const functionsPermissionsReducer = (
  state: ReducerState,
  action: ReducerAction
) => {
  switch (action && action.type) {
    case PERM_UPDATE_OPEN_STATE:
      return {
        isEditing: true,
        role: action?.role,
      };
    case PERM_UPDATE_CLOSE_STATE:
      return {
        ...state,
        isEditing: false,
      };
    default:
      return state;
  }
};

const PermissionsLegend = () => (
  <div className={styles.permissionsLegend}>
    <span className={styles.permissionsLegendValue}>
      {permissionsSymbols.fullAccess} : allowed
    </span>
    <span className={styles.permissionsLegendValue}>
      {permissionsSymbols.noAccess} : not allowed
    </span>
    <span className={styles.permissionsLegendValue}>
      {permissionsSymbols.partialAccessWarning} : partial (needs SELECT
      permissions on table)
    </span>
  </div>
);

const getPermissionAccessString = (permissionSymbol: JSX.Element) => {
  if (permissionSymbol === permissionsSymbols.fullAccess) {
    return 'full';
  } else if (permissionSymbol === permissionsSymbols.noAccess) {
    return 'no';
  }

  return 'partial';
};

const EditIcon = () => (
  <span className={styles.editPermsIcon}>
    <i className="fa fa-pencil" aria-hidden="true" />
  </span>
);

const PermissionsTableBody: React.FC<PermissionTableProps> = ({
  allPermissions,
  allRoles,
  permCloseEdit,
  permOpenEdit,
  permissionsEditState,
  readOnlyMode,
  isEditable,
  selectRoles,
}) => {
  const queryTypes = ['Permission'];
  const { isEditing, role: permEditRole } = permissionsEditState;

  const getQueryTypes = (role: string) =>
    queryTypes.map(queryType => {
      const dispatchOpenEdit = (r: string) => () => {
        if (r) {
          permOpenEdit(r);
        }
      };

      const isCurrEdit = isEditing && permEditRole === role;
      let editIcon = null;
      let className = '';
      let onClick = () => {};
      const tooltip =
        !isEditable && role !== 'admin' ? (
          <Tooltip message="Forbidden from edits since function permissions are inferred" />
        ) : null;

      if (role !== 'admin' && !readOnlyMode && isEditable) {
        editIcon = <EditIcon />;
        if (isCurrEdit) {
          onClick = permCloseEdit;
          className += ` ${styles.currEdit}`;
        } else {
          className += styles.clickableCell;
          onClick = dispatchOpenEdit(role);
        }
      }

      return {
        permType: queryType,
        className,
        editIcon,
        onClick,
        dataTest: `${role}-${queryType}`,
        access: getRoleQueryPermissionSymbol(
          allPermissions,
          role,
          selectRoles,
          isEditable
        ),
        tooltip,
      };
    });

  const roleList = ['admin', ...allRoles];
  const rolePermissions = roleList.map(r => ({
    roleName: r,
    permTypes: getQueryTypes(r),
  }));

  return (
    <PermTableBody
      rolePermissions={rolePermissions}
      dispatchRoleNameChange={() => {}}
    />
  );
};

type PermissionTableProps = {
  permCloseEdit: () => void;
  permOpenEdit: (role: string) => void;
  permissionsEditState: ReducerState;
  allRoles: string[];
  readOnlyMode: boolean;
  allPermissions: FunctionPermission[] | undefined | null;
  isEditable: boolean;
  selectRoles: SelectPermissionEntry[] | null;
};
const PermissionsTable: React.FC<PermissionTableProps> = props => (
  <>
    <PermissionsLegend />
    <table className={`table table-bordered ${styles.permissionsTable}`}>
      <PermTableHeader headings={['Role', 'Permission']} />
      <PermissionsTableBody {...props} />
    </table>
  </>
);

interface PermissionsProps extends InjectedProps {
  currentSchema: string;
  currentFunctionName: string;
  isPermissionsEditable: boolean;
}
const Permissions: React.FC<PermissionsProps> = ({
  allFunctions,
  dispatch,
  currentSchema,
  currentFunctionName,
  allRoles,
  readOnlyMode = false,
  tableSelectPermissions,
  isPermissionsEditable,
  currentTable,
}) => {
  const [permissionsEditState, permissionsDispatch] = useReducer(
    functionsPermissionsReducer,
    initialState
  );

  const { isEditing, role: permEditRole } = permissionsEditState;

  const permCloseEdit = () => {
    permissionsDispatch({
      type: PERM_UPDATE_CLOSE_STATE,
      role: '',
    });
  };
  const permOpenEdit = (role: string) => {
    permissionsDispatch({
      type: PERM_UPDATE_OPEN_STATE,
      role,
    });
  };

  const allPermissions = getFunctionPermissions(
    allFunctions,
    currentSchema,
    currentFunctionName
  );

  const permissionAccessString = getPermissionAccessString(
    getRoleQueryPermissionSymbol(
      allPermissions,
      permEditRole,
      tableSelectPermissions,
      isPermissionsEditable
    )
  );

  const saveFunc = () =>
    dispatch(setFunctionPermission(permEditRole, permCloseEdit));
  const removeFunc = () =>
    dispatch(dropFunctionPermission(permEditRole, permCloseEdit));

  return (
    <>
      <PermissionsTable
        permCloseEdit={permCloseEdit}
        permOpenEdit={permOpenEdit}
        permissionsEditState={permissionsEditState}
        allRoles={allRoles}
        readOnlyMode={readOnlyMode}
        allPermissions={allPermissions}
        isEditable={isPermissionsEditable}
        selectRoles={tableSelectPermissions}
      />
      <div className={styles.add_mar_bottom}>
        {!readOnlyMode && (
          <PermissionEditor
            saveFn={saveFunc}
            removeFn={removeFunc}
            closeFn={permCloseEdit}
            role={permEditRole}
            isEditing={isEditing}
            permissionAccessInMetadata={permissionAccessString}
            table={currentTable}
          />
        )}
      </div>
    </>
  );
};

const mapStateToProps = (state: ReduxState) => {
  const { setOffTable, setOffTableSchema } = state.functions;
  return {
    allRoles: rolesSelector(state),
    allFunctions: getFunctions(state),
    functions: state.functions,
    readOnlyMode: state.main.readOnlyMode,
    tableSelectPermissions:
      getTableInformation(state)(setOffTable, setOffTableSchema)(
        'select_permissions'
      ) ?? [],
    currentTable: setOffTable,
  };
};

const permissionsUIConnector = connect(
  mapStateToProps,
  mapDispatchToPropsEmpty
);
type InjectedProps = ConnectedProps<typeof permissionsUIConnector>;
export default permissionsUIConnector(Permissions);
