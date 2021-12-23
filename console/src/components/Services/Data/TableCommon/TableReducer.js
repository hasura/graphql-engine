import { defaultModifyState, defaultPermissionsState } from '../DataState';

import { MAKE_REQUEST, REQUEST_SUCCESS, REQUEST_ERROR } from '../DataActions';

// TABLE MODIFY

import {
  VIEW_DEF_REQUEST_SUCCESS,
  VIEW_DEF_REQUEST_ERROR,
  TABLE_COMMENT_EDIT,
  TABLE_COMMENT_INPUT_EDIT,
  SET_COLUMN_EDIT,
  RESET_COLUMN_EDIT,
  EDIT_COLUMN,
  SET_PRIMARY_KEYS,
  SET_FOREIGN_KEYS,
  FETCH_COLUMN_TYPE_CASTS,
  FETCH_COLUMN_TYPE_CASTS_FAIL,
  RESET,
  SET_UNIQUE_KEYS,
  TOGGLE_ENUM,
  TOGGLE_ENUM_SUCCESS,
  TOGGLE_ENUM_FAILURE,
  MODIFY_ROOT_FIELD,
  MODIFY_TABLE_CUSTOM_NAME,
  SET_CHECK_CONSTRAINTS,
  SET_VIEW_DEF_SQL,
} from '../TableModify/ModifyActions';

// TABLE RELATIONSHIPS

import {
  MANUAL_REL_SET_TYPE,
  MANUAL_REL_SET_RSCHEMA,
  MANUAL_REL_SET_RTABLE,
  MANUAL_REL_NAME_CHANGED,
  SET_MANUAL_REL_ADD,
  REL_NAME_CHANGED,
  REL_RESET,
  MANUAL_REL_RESET,
  REL_SELECTION_CHANGED,
  REL_ADD_NEW_CLICKED,
  SET_REMOTE_RELATIONSHIPS,
} from '../TableRelationships/Actions';

// TABLE PERMISSIONS

import {
  PERM_OPEN_EDIT,
  PERM_SET_FILTER,
  PERM_SET_FILTER_SAME_AS,
  PERM_TOGGLE_FIELD,
  PERM_TOGGLE_ALL_FIELDS,
  PERM_ALLOW_ALL,
  PERM_TOGGLE_MODIFY_LIMIT,
  PERM_TOGGLE_ALLOW_UPSERT,
  PERM_TOGGLE_ALLOW_AGGREGATION,
  PERM_CUSTOM_CHECKED,
  PERM_REMOVE_ACCESS,
  PERM_SAVE_PERMISSIONS,
  PERM_CLOSE_EDIT,
  PERM_SET_ROLE_NAME,
  PERM_SELECT_BULK,
  PERM_DESELECT_BULK,
  PERM_RESET_BULK_SELECT,
  PERM_RESET_APPLY_SAME,
  PERM_SET_APPLY_SAME_PERM,
  PERM_DEL_APPLY_SAME_PERM,
  PERM_TOGGLE_BACKEND_ONLY,
  toggleField,
  toggleAllFields,
  getBasePermissionsState,
  updatePermissionsState,
  deleteFromPermissionsState,
  updateBulkSelect,
  updateApplySamePerms,
  DELETE_PRESET,
  SET_PRESET_VALUE,
} from '../TablePermissions/Actions';
import { getDefaultFilterType } from '../TablePermissions/utils';

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
    case SET_VIEW_DEF_SQL:
      return {
        ...modifyState,
        viewDefSql: action.data,
      };

    case REL_ADD_NEW_CLICKED:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          isActive: true,
        },
      };
    case REL_NAME_CHANGED:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          relName: action.relName,
        },
      };
    case MANUAL_REL_NAME_CHANGED:
      return {
        ...modifyState,
        manualRelAdd: {
          ...modifyState.manualRelAdd,
          relName: action.relName,
        },
      };
    case REL_SELECTION_CHANGED:
      const selectedRel = action.rel;
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          relName: '',
          lTable: selectedRel.lTable,
          lSchema: selectedRel.lSchema,
          isObjRel: selectedRel.isObjRel,
          lcol: selectedRel.lcol,
          rTable: selectedRel.rTable,
          rSchema: selectedRel.rSchema,
          rcol: selectedRel.rcol,
          isUnique: selectedRel.isUnique,
          isPrimary: selectedRel.isPrimary,
        },
      };
    case REL_RESET:
      return {
        ...modifyState,
        relAdd: {
          ...defaultModifyState.relAdd,
        },
      };
    case MANUAL_REL_RESET:
      return {
        ...modifyState,
        manualRelAdd: {
          ...defaultModifyState.manualRelAdd,
        },
      };
    case MANUAL_REL_SET_TYPE:
      return {
        ...modifyState,
        manualRelAdd: {
          ...modifyState.manualRelAdd,
          relType: action.relType,
        },
      };
    case MANUAL_REL_SET_RSCHEMA:
      return {
        ...modifyState,
        manualRelAdd: {
          ...modifyState.manualRelAdd,
          rSchema: action.rSchema,
          rTable: '',
          colMappings: [...defaultModifyState.manualRelAdd.colMappings],
        },
      };
    case MANUAL_REL_SET_RTABLE:
      return {
        ...modifyState,
        manualRelAdd: {
          ...modifyState.manualRelAdd,
          rTable: action.rTable,
          colMappings: [...defaultModifyState.manualRelAdd.colMappings],
        },
      };
    case SET_MANUAL_REL_ADD:
      return {
        ...modifyState,
        manualRelAdd: action.manualRelAdd,
      };
    case TABLE_COMMENT_EDIT:
      return {
        ...modifyState,
        tableCommentEdit: {
          ...modifyState.tableCommentEdit,
          enabled: action.data.enabled,
          editedValue: action.data.value,
        },
      };
    case TABLE_COMMENT_INPUT_EDIT:
      return {
        ...modifyState,
        tableCommentEdit: {
          ...modifyState.tableCommentEdit,
          editedValue: action.value,
        },
      };
    case SET_COLUMN_EDIT:
      return {
        ...modifyState,
        columnEdit: {
          ...modifyState.columnEdit,
          [action.column]: { ...action.data },
        },
      };
    case RESET_COLUMN_EDIT:
      const updatedColumnEdit = {
        ...modifyState.columnEdit,
      };
      delete updatedColumnEdit[action.column];
      return {
        ...modifyState,
        columnEdit: updatedColumnEdit,
      };
    case EDIT_COLUMN:
      return {
        ...modifyState,
        columnEdit: {
          ...modifyState.columnEdit,
          [action.column]: {
            ...modifyState.columnEdit[action.column],
            [action.key]: action.value,
          },
        },
      };
    case PERM_OPEN_EDIT:
      const isNewRole = modifyState.permissionsState.newRole === action.role;
      const permState = getBasePermissionsState(
        action.tableSchema,
        action.role,
        action.query,
        isNewRole
      );
      return {
        ...modifyState,
        permissionsState: {
          ...permState,
          isEditing: true,
        },
        prevPermissionState: {
          ...permState,
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
          custom_checked: {
            ...modifyState.permissionsState.custom_checked,
            [action.filterType]: true,
          },
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

    case PERM_TOGGLE_ALLOW_AGGREGATION:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            'allow_aggregations',
            action.data
          ),
        },
      };

    case PERM_TOGGLE_MODIFY_LIMIT:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            'limit',
            action.data
          ),
        },
      };

    case PERM_SET_FILTER_SAME_AS:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            action.filterType ||
              getDefaultFilterType(modifyState.permissionsState.query),
            action.filter
          ),
          custom_checked: {
            ...modifyState.permissionsState.custom_checked,
            [action.filterType]: false,
          },
        },
      };
    case PERM_SET_FILTER:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            action.filterType ||
              getDefaultFilterType(modifyState.permissionsState.query),
            action.filter
          ),
        },
      };

    case PERM_ALLOW_ALL:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            action.filterType ||
              getDefaultFilterType(modifyState.permissionsState.query),
            {}
          ),
          custom_checked: {
            ...modifyState.permissionsState.custom_checked,
            [action.filterType]: false,
          },
        },
      };

    case PERM_TOGGLE_ALL_FIELDS:
      let returnState = {
        ...modifyState,
      };

      Object.keys(action.allFields).forEach(fieldType => {
        returnState = {
          ...returnState,
          permissionsState: {
            ...updatePermissionsState(
              returnState.permissionsState,
              fieldType,
              toggleAllFields(
                modifyState.permissionsState[
                  modifyState.permissionsState.query
                ],
                action.allFields,
                fieldType
              )
            ),
          },
        };
      });

      return returnState;

    case PERM_TOGGLE_FIELD:
      return {
        ...modifyState,
        permissionsState: {
          ...updatePermissionsState(
            modifyState.permissionsState,
            action.fieldType,
            toggleField(
              modifyState.permissionsState[modifyState.permissionsState.query],
              action.fieldName,
              action.fieldType
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
      const newRole = action.data;
      const role = modifyState.permissionsState.isEditing
        ? newRole
        : modifyState.permissionsState.role;

      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          newRole: newRole,
          role: role,
        },
      };

    case PERM_SAVE_PERMISSIONS:
      return {
        ...modifyState,
      };

    case PERM_SELECT_BULK:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          bulkSelect: updateBulkSelect(
            modifyState.permissionsState,
            action.data,
            true
          ),
        },
      };
    case PERM_DESELECT_BULK:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          bulkSelect: updateBulkSelect(
            modifyState.permissionsState,
            action.data,
            false
          ),
        },
      };
    case PERM_SET_APPLY_SAME_PERM:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          applySamePermissions: updateApplySamePerms(
            modifyState.permissionsState,
            action.data
          ),
        },
      };
    case PERM_DEL_APPLY_SAME_PERM:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          applySamePermissions: updateApplySamePerms(
            modifyState.permissionsState,
            action.data,
            true
          ),
        },
      };
    case PERM_TOGGLE_BACKEND_ONLY:
      const pState = modifyState.permissionsState;
      const isBackendOnly =
        pState[pState.query] && pState[pState.query].backend_only;
      return {
        ...modifyState,
        permissionsState: updatePermissionsState(
          modifyState.permissionsState,
          'backend_only',
          !isBackendOnly
        ),
      };
    /* Preset operations */
    case DELETE_PRESET:
      const deletedSet = {
        ...modifyState.permissionsState[action.data.queryType].set,
      };
      delete deletedSet[action.data.column];

      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          [action.data.queryType]: {
            ...modifyState.permissionsState[action.data.queryType],
            set: deletedSet,
          },
        },
      };

    case SET_PRESET_VALUE:
      const updatedSet = {
        ...modifyState.permissionsState[action.data.queryType].set,
      };
      if (action.data.prevKey) {
        updatedSet[action.data.column] = updatedSet[action.data.prevKey];
        delete updatedSet[action.data.prevKey];
      } else {
        updatedSet[action.data.column] = action.data.value || '';
      }

      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          [action.data.queryType]: {
            ...modifyState.permissionsState[action.data.queryType],
            set: updatedSet,
          },
        },
      };

    case PERM_RESET_BULK_SELECT:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          bulkSelect: [],
        },
      };
    case PERM_RESET_APPLY_SAME:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          applySamePermissions: [],
        },
      };

    case SET_PRIMARY_KEYS:
      return {
        ...modifyState,
        pkModify: action.pks,
      };
    case SET_FOREIGN_KEYS:
      return {
        ...modifyState,
        fkModify: action.fks,
      };

    case FETCH_COLUMN_TYPE_CASTS:
      return {
        ...modifyState,
        alterColumnOptions: action.data,
        alterColumnOptionsFetchErr: null,
      };

    case FETCH_COLUMN_TYPE_CASTS_FAIL:
      return {
        ...modifyState,
        alterColumnOptions: [],
        alterColumnOptionsFetchErr: action.data,
      };

    case SET_UNIQUE_KEYS:
      return {
        ...modifyState,
        uniqueKeyModify: action.keys,
      };

    case SET_REMOTE_RELATIONSHIPS:
      return {
        ...modifyState,
        remoteRelationships: {
          ...modifyState.remoteRelationships,
          relationships: action.remoteRelationships,
        },
      };
    case TOGGLE_ENUM:
      return {
        ...modifyState,
        tableEnum: {
          loading: true,
        },
      };
    case TOGGLE_ENUM_FAILURE:
      return {
        ...modifyState,
        tableEnum: {
          loading: false,
          error: action.error,
        },
      };
    case TOGGLE_ENUM_SUCCESS:
      return {
        ...modifyState,
        tableEnum: {
          loading: false,
        },
      };
    case MODIFY_TABLE_CUSTOM_NAME:
      return {
        ...modifyState,
        custom_name: action.data,
      };
    case MODIFY_ROOT_FIELD:
      return {
        ...modifyState,
        rootFieldsEdit: action.data,
      };
    case SET_CHECK_CONSTRAINTS:
      return {
        ...modifyState,
        checkConstraintsModify: action.constraints,
      };
    default:
      return modifyState;
  }
};

export default modifyReducer;
