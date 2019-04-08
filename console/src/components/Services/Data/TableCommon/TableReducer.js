import {
  defaultModifyState,
  defaultPermissionsState,
  defaultSetState,
} from '../DataState';

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
  RESET,
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
  PERM_ADD_TABLE_SCHEMAS,
  PERM_SET_FILTER,
  PERM_SET_FILTER_SAME_AS,
  PERM_TOGGLE_COLUMN,
  PERM_TOGGLE_ALL_COLUMNS,
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
  PERM_RESET_BULK_SAME_SELECT,
  PERM_SAME_APPLY_BULK,
  PERM_DESELECT_SAME_APPLY_BULK,
  toggleColumn,
  toggleAllColumns,
  getFilterKey,
  getBasePermissionsState,
  updatePermissionsState,
  deleteFromPermissionsState,
  updateBulkSelect,
  updateBulkSameSelect,
  CREATE_NEW_SET_VAL,
  DELETE_SET_VAL,
  UPDATE_PERM_SET_KEY_VALUE,
  TOGGLE_PERM_SET_OPERATION_CHECK,
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
          isManualExpanded: !modifyState.relAdd.isManualExpanded,
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
          ...defaultModifyState.relAdd,
        },
      };
    case REL_SET_TYPE:
      return {
        ...modifyState,
        relAdd: {
          ...modifyState.relAdd,
          isObjRel: action.isObjRel,
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
      const permState = getBasePermissionsState(
        action.tableSchema,
        action.role,
        action.query,
        action.insertPermColumnRestriction
      );
      return {
        ...modifyState,
        permissionsState: {
          ...permState,
          tableSchemas: schemas,
        },
        prevPermissionState: {
          ...permState,
        },
      };

    case PERM_ADD_TABLE_SCHEMAS:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          tableSchemas: [
            ...modifyState.permissionsState.tableSchemas,
            ...action.schemas,
          ],
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
    case PERM_SAME_APPLY_BULK:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          applySamePermissions: updateBulkSameSelect(
            modifyState.permissionsState,
            action.data,
            true
          ),
        },
      };
    case PERM_DESELECT_SAME_APPLY_BULK:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          applySamePermissions: updateBulkSameSelect(
            modifyState.permissionsState,
            action.data,
            false
          ),
        },
      };

    /* Set operations */
    case TOGGLE_PERM_SET_OPERATION_CHECK:
      if (
        modifyState.permissionsState[action.data.queryType].localSet.length ===
          0 ||
        (modifyState.permissionsState[action.data.queryType].localSet.length ===
          1 &&
          modifyState.permissionsState[action.data.queryType].localSet[0]
            .key === '')
      ) {
        return {
          ...modifyState,
          permissionsState: {
            ...modifyState.permissionsState,
            [action.data.queryType]: {
              ...modifyState.permissionsState[action.data.queryType],
              isSetConfigChecked: !modifyState.permissionsState[
                action.data.queryType
              ].isSetConfigChecked,
            },
          },
        };
      }
      return modifyState;
    case CREATE_NEW_SET_VAL:
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          [action.data.queryType]: {
            ...modifyState.permissionsState[action.data.queryType],
            localSet: [
              ...modifyState.permissionsState[action.data.queryType].localSet,
              { ...defaultSetState[action.data.queryType] },
            ],
          },
        },
      };
    case DELETE_SET_VAL:
      const deleteIndex = action.data.index;
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          [action.data.queryType]: {
            ...modifyState.permissionsState[action.data.queryType],
            localSet: [
              ...modifyState.permissionsState[
                action.data.queryType
              ].localSet.slice(0, deleteIndex),
              ...modifyState.permissionsState[
                action.data.queryType
              ].localSet.slice(
                deleteIndex + 1,
                modifyState.permissionsState[action.data.queryType].localSet
                  .length
              ),
            ],
          },
        },
      };

    case UPDATE_PERM_SET_KEY_VALUE:
      const updatedIndex = action.data.index;
      const setKeyVal =
        modifyState.permissionsState[action.data.queryType].localSet[
          updatedIndex
        ];
      setKeyVal[action.data.key] = action.data.value;
      if (action.data.key === 'key') {
        // Clear if key changes
        setKeyVal.value = '';
      }
      return {
        ...modifyState,
        permissionsState: {
          ...modifyState.permissionsState,
          [action.data.queryType]: {
            ...modifyState.permissionsState[action.data.queryType],
            localSet: [
              ...modifyState.permissionsState[
                action.data.queryType
              ].localSet.slice(0, updatedIndex),
              { ...setKeyVal },
              ...modifyState.permissionsState[
                action.data.queryType
              ].localSet.slice(
                updatedIndex + 1,
                modifyState.permissionsState[action.data.queryType].localSet
                  .length
              ),
            ],
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
    case PERM_RESET_BULK_SAME_SELECT:
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
    default:
      return modifyState;
  }
};

export default modifyReducer;
