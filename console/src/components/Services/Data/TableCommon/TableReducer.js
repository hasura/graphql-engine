import { defaultModifyState, defaultPermissionsState } from '../DataState';

import { MAKE_REQUEST, REQUEST_SUCCESS, REQUEST_ERROR } from '../DataActions';

// TABLE MODIFY

import {
  TOGGLE_ACTIVE_COLUMN,
  RESET,
  VIEW_DEF_REQUEST_SUCCESS,
  VIEW_DEF_REQUEST_ERROR,
  FK_SET_REF_TABLE,
  FK_SET_L_COL,
  FK_SET_R_COL,
  FK_ADD_FORM_ERROR,
  FK_RESET,
  TOGGLE_FK_CHECKBOX,
} from '../TableModify/ModifyActions';

// TABLE RELATIONSHIPS

import {
  REL_SET_TYPE,
  REL_SET_RTABLE,
  REL_SET_LCOL,
  REL_SET_RCOL,
  REL_RESET,
  REL_SELECTION_CHANGED,
  REL_NAME_CHANGED,
  REL_ADD_NEW_CLICKED,
  REL_SET_MANUAL_COLUMNS,
  REL_ADD_MANUAL_CLICKED,
} from '../TableRelationships/Actions';

// TABLE PERMISSIONS

import {
  PERM_OPEN_EDIT,
  PERM_SET_FILTER,
  PERM_SET_FILTER_SAME_AS,
  PERM_TOGGLE_COLUMN,
  PERM_TOGGLE_ALL_COLUMNS,
  PERM_ALLOW_ALL,
  PERM_TOGGLE_ALLOW_UPSERT,
  PERM_CUSTOM_CHECKED,
  PERM_REMOVE_ACCESS,
  PERM_SAVE_PERMISSIONS,
  PERM_CLOSE_EDIT,
  PERM_SET_ROLE_NAME,
  toggleColumn,
  toggleAllColumns,
  getFilterKey,
  getBasePermissionsState,
  updatePermissionsState,
  deleteFromPermissionsState,
} from '../TablePermissions/Actions';

const modifyReducer = (tableName, schemas, modifyStateOrig, action) => {
  const modifyState = JSON.parse(JSON.stringify(modifyStateOrig));

  switch (action.type) {
    case RESET:
      return { ...defaultModifyState };
    case MAKE_REQUEST:
      return {
        ...modifyState,
        ongoingRequest: true,
        lastError: null,
        lastSuccess: null,
      };
    case REQUEST_SUCCESS:
      return {
        ...modifyState,
        ongoingRequest: false,
        lastError: null,
        lastSuccess: true,
      };
    case REQUEST_ERROR:
      return {
        ...modifyState,
        ongoingRequest: false,
        lastError: action.data,
        lastSuccess: false,
      };
    case VIEW_DEF_REQUEST_SUCCESS:
      return {
        ...modifyState,
        viewDefinition: action.data,
        viewDefinitionError: action.error,
      };
    case VIEW_DEF_REQUEST_ERROR:
      return {
        ...modifyState,
        viewDefinitionError: action.data,
      };

    case REL_ADD_NEW_CLICKED:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          isActive: true,
        },
      };
    case REL_ADD_MANUAL_CLICKED:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          isManualExpanded: true,
        },
      };
    case REL_NAME_CHANGED:
      const relName = action.relName;
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          name: relName,
        },
      };
    case REL_SELECTION_CHANGED:
      const selectedRel = action.rel;
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          name: '',
          tableName: selectedRel.tableName,
          isObjRel: selectedRel.isObjRel,
          lcol: selectedRel.lcol,
          rTable: selectedRel.rTable,
          rcol: selectedRel.rcol,
        },
      };
    case REL_RESET:
      return {
        ...modifyState,
        relAdd: {
          isActive: true,
          tableName: '',
          name: '',
          isObjRel: null,
          lcol: '',
          rTable: null,
          rcol: '',
          manualColumns: [],
        },
      };
    case REL_SET_TYPE:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          isObjRel: action.isObjRel,
          rTable: null,
          rcol: '',
        },
      };
    case REL_SET_RTABLE:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          rTable: action.rTable,
        },
      };
    case REL_SET_LCOL:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          lcol: action.lcol,
        },
      };
    case REL_SET_RCOL:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          rcol: action.rcol,
        },
      };
    case REL_SET_MANUAL_COLUMNS:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          manualColumns: action.data,
        },
      };

    case FK_RESET:
      return {
        ...modifyState,
        fkAdd: {
          ...modifyState.fkAdd,
          pairs: [],
          lcol: '',
          rcol: '',
          refTable: '',
          fkCheckBox: false,
        },
      };
    case FK_SET_REF_TABLE:
      return {
        ...modifyState,
        fkAdd: {
          ...modifyState.fkAdd,
          refTable: action.refTable,
          rcol: '',
        },
      };
    case FK_SET_L_COL:
      return {
        ...modifyState,
        fkAdd: {
          ...modifyState.fkAdd,
          lcol: action.lcol,
        },
      };
    case FK_SET_R_COL:
      return {
        ...modifyState,
        fkAdd: {
          ...modifyState.fkAdd,
          rcol: action.rcol,
        },
      };
    case FK_ADD_FORM_ERROR:
      return {
        ...modifyState,
        ongoingRequest: false,
        lastError: null,
        lastFormError: action.errorMessage,
        lastSuccess: false,
      };
    case TOGGLE_FK_CHECKBOX:
      return {
        ...modifyState,
        fkAdd: {
          ...modifyState.fkAdd,
          fkCheckBox: action.checked,
        },
      };

    case PERM_OPEN_EDIT:
      return {
        ...modifyState,
        permissionsState: {
          ...getBasePermissionsState(
            action.tableSchema,
            action.role,
            action.query
          ),
        },
      };

    case PERM_CLOSE_EDIT:
      return {
        ...modifyState,
        permissionsState: {
          ...JSON.parse(JSON.stringify(defaultPermissionsState)),
        },
      };

    case PERM_CUSTOM_CHECKED:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          custom_checked: true,
        },
      };

    case PERM_TOGGLE_ALLOW_UPSERT:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            'allow_upsert',
            action.data
          ),
        },
      };

    case PERM_SET_FILTER_SAME_AS:
    case PERM_SET_FILTER:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            getFilterKey(modifyState.permissionsState.query),
            action.filter
          ),
          custom_checked: false,
        },
      };

    case PERM_ALLOW_ALL:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            getFilterKey(modifyState.permissionsState.query),
            {}
          ),
          custom_checked: false,
        },
      };

    case PERM_TOGGLE_ALL_COLUMNS:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            'columns',
            toggleAllColumns(
              modifyState.permissionsState[modifyState.permissionsState.query],
              action.allColumns
            )
          ),
        },
      };

    case PERM_TOGGLE_COLUMN:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            'columns',
            toggleColumn(
              modifyState.permissionsState[modifyState.permissionsState.query],
              action.column
            )
          ),
        },
      };

    case PERM_REMOVE_ACCESS:
      return {
        ...modifyState,
        permissionsState: {
          ...deleteFromPermissionsState(modifyState.permissionsState),
        },
      };

    case PERM_SET_ROLE_NAME:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          newRole: action.data,
        },
      };

    case PERM_SAVE_PERMISSIONS:
      return {
        ...modifyState,
      };

    case TOGGLE_ACTIVE_COLUMN:
      const newCol =
        modifyState.activeEdit.column === action.column ? '' : action.column;
      return {
        ...modifyState,
        activeEdit: { ...modifyState.activeEdit, column: newCol },
      };

    default:
      return modifyState;
  }
};

export default modifyReducer;
