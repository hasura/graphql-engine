import requestAction from '../../../../utils/requestAction';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import { CLI_CONSOLE_MODE } from '../../../../constants';
import {
  updateSchemaInfo,
  handleMigrationErrors,
  makeMigrationCall,
  LOAD_SCHEMA,
} from '../DataActions';
import _push from '../push';
import {
  showErrorNotification,
  showSuccessNotification,
  showWarningNotification,
} from '../../Common/Notification';
import dataHeaders from '../Common/Headers';
import { UPDATE_MIGRATION_STATUS_ERROR } from '../../../Main/Actions';
import gqlPattern, {
  gqlTableErrorNotif,
  gqlViewErrorNotif,
  gqlColumnErrorNotif,
} from '../Common/GraphQLValidation';
import {
  pgConfTypes,
  generateFKConstraintName,
  getUniqueConstraintName,
} from '../Common/Components/utils';

import { getConfirmation, capitalize } from '../../../Common/utils/jsUtils';
import {
  findTable,
  findTableCheckConstraint,
  getTableCustomRootFields,
  getTableCustomColumnNames,
  getTableDef,
  generateTableDef,
  dataSource,
} from '../../../../dataSources';
import {
  getSetCustomRootFieldsQuery,
  getRunSqlQuery,
  getDropComputedFieldQuery,
  getAddComputedFieldQuery,
  getSetTableEnumQuery,
  getUntrackTableQuery,
  getTrackTableQuery,
} from '../../../Common/utils/v1QueryUtils';
import {
  convertArrayToJson,
  sanitiseRootFields,
  sanitiseColumnNames,
} from './utils';
import {
  getSchemaBaseRoute,
  getTableModifyRoute,
} from '../../../Common/utils/routesUtils';
import { exportMetadata } from '../../../../metadata/actions';

const DELETE_PK_WARNING =
  'Without a primary key there is no way to uniquely identify a row of a table';

const VIEW_DEF_REQUEST_SUCCESS = 'ModifyTable/VIEW_DEF_REQUEST_SUCCESS';
const VIEW_DEF_REQUEST_ERROR = 'ModifyTable/VIEW_DEF_REQUEST_ERROR';
const SET_VIEW_DEF_SQL = 'ModifyTable/SET_VIEW_DEF_SQL';

const SAVE_NEW_TABLE_NAME = 'ModifyTable/SAVE_NEW_TABLE_NAME';

const TABLE_COMMENT_EDIT = 'ModifyTable/TABLE_COMMENT_EDIT';
const TABLE_COMMENT_INPUT_EDIT = 'ModifyTable/TABLE_COMMENT_INPUT_EDIT';

const ADD_PRIMARY_KEY = 'ModifyTable/ADD_PRIMARY_KEY';
const REMOVE_PRIMARY_KEY = 'ModifyTable/REMOVE_PRIMARY_KEY';
const RESET_PRIMARY_KEY = 'ModifyTable/RESET_PRIMARY_KEY';
const SET_PRIMARY_KEYS = 'ModifyTable/SET_PRIMARY_KEYS';

const SET_COLUMN_EDIT = 'ModifyTable/SET_COLUMN_EDIT';
const RESET_COLUMN_EDIT = 'ModifyTable/RESET_COLUMN_EDIT';
const EDIT_COLUMN = 'ModifyTable/EDIT_COLUMN';

const SET_FOREIGN_KEYS = 'ModifyTable/SET_FOREIGN_KEYS';
const SAVE_FOREIGN_KEY = 'ModifyTable/SAVE_FOREIGN_KEY';
const REMOVE_FOREIGN_KEY = 'ModifyTable/REMOVE_FOREIGN_KEY';

const FETCH_COLUMN_TYPE_CASTS = 'ModifyTable/FETCH_COLUMN_TYPE_CASTS';
const FETCH_COLUMN_TYPE_CASTS_FAIL = 'ModifyTable/FETCH_COLUMN_TYPE_CASTS_FAIL';
const SET_UNIQUE_KEYS = 'ModifyTable/SET_UNIQUE_KEYS';
const SAVE_UNIQUE_KEY = 'ModifyTable/SAVE_UNIQUE_KEY';
const REMOVE_UNIQUE_KEY = 'ModifyTable/REMOVE_UNIQUE_KEY';
const TOGGLE_ENUM = 'ModifyTable/TOGGLE_ENUM';
const TOGGLE_ENUM_SUCCESS = 'ModifyTable/TOGGLE_ENUM_SUCCESS';
const TOGGLE_ENUM_FAILURE = 'ModifyTable/TOGGLE_ENUM_FAILURE';

const MODIFY_ROOT_FIELD = 'ModifyTable/MODIFY_ROOT_FIELD';
const SET_CUSTOM_ROOT_FIELDS = 'ModifyTable/SET_CUSTOM_ROOT_FIELDS';

const SET_CHECK_CONSTRAINTS = 'ModifyTable/SET_CHECK_CONSTRAINTS';
const setCheckConstraints = constraints => ({
  type: SET_CHECK_CONSTRAINTS,
  constraints,
});

const RESET = 'ModifyTable/RESET';

const toggleEnumSuccess = () => ({
  type: TOGGLE_ENUM_SUCCESS,
});

const toggleEnumFailure = () => ({
  type: TOGGLE_ENUM_FAILURE,
});

const setForeignKeys = fks => ({
  type: SET_FOREIGN_KEYS,
  fks,
});

const modifyRootFields = rootFields => ({
  type: MODIFY_ROOT_FIELD,
  data: rootFields,
});

const editColumn = (column, key, value) => ({
  type: EDIT_COLUMN,
  column,
  key,
  value,
});

const setColumnEdit = data => {
  return {
    type: SET_COLUMN_EDIT,
    column: data.name,
    data,
  };
};

const resetColumnEdit = column => {
  return {
    type: RESET_COLUMN_EDIT,
    column,
  };
};

const setPrimaryKeys = pks => ({
  type: SET_PRIMARY_KEYS,
  pks,
});

const addPrimaryKey = (columnIndex, pkIndex) => ({
  type: ADD_PRIMARY_KEY,
  column: columnIndex,
  pk: pkIndex,
});

const removePrimaryKey = pkIndex => ({
  type: REMOVE_PRIMARY_KEY,
  pk: pkIndex,
});

const resetPrimaryKeys = () => ({
  type: RESET_PRIMARY_KEY,
});

export const saveComputedField = (
  computedField,
  table,
  originalComputedField,
  successCb
) => (dispatch, getState) => {
  const migrationUp = [];
  const migrationDown = [];

  const tableDef = getTableDef(table);

  const computedFieldName = computedField.computed_field_name;

  if (originalComputedField) {
    migrationUp.push(
      getDropComputedFieldQuery(
        tableDef,
        originalComputedField.computed_field_name
      )
    );
  }

  migrationUp.push(
    getAddComputedFieldQuery(
      tableDef,
      computedFieldName,
      computedField.definition,
      computedField.comment
    )
  );

  migrationDown.push(getDropComputedFieldQuery(tableDef, computedFieldName));

  if (originalComputedField) {
    migrationDown.push(
      getAddComputedFieldQuery(
        tableDef,
        originalComputedField.computed_field_name,
        originalComputedField.definition,
        originalComputedField.comment
      )
    );
  }

  const migrationName = `save_computed_field_${computedField.table_schema}_${computedField.table_name}_${computedFieldName}`;
  const requestMsg = 'Saving computed field...';
  const successMsg = 'Saving computed field successful';
  const errorMsg = 'Saving computed field failed';
  const customOnSuccess = () => {
    successCb();
  };
  const customOnError = () => {};

  makeMigrationCall(
    dispatch,
    getState,
    migrationUp,
    migrationDown,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export const deleteComputedField = (computedField, table) => (
  dispatch,
  getState
) => {
  const migrationUp = [];
  const migrationDown = [];

  const tableDef = getTableDef(table);
  const computedFieldName = computedField.computed_field_name;

  migrationUp.push(getDropComputedFieldQuery(tableDef, computedFieldName));

  migrationDown.push(
    getAddComputedFieldQuery(
      tableDef,
      computedFieldName,
      computedFieldName.definition,
      computedField.comment
    )
  );

  const migrationName = `delete_computed_field_${computedField.table_schema}_${computedField.table_name}_${computedFieldName}`;
  const requestMsg = 'Deleting computed field...';
  const successMsg = 'Deleting computed field successful';
  const errorMsg = 'Deleting computed field failed';
  const customOnSuccess = () => {};
  const customOnError = () => {};

  makeMigrationCall(
    dispatch,
    getState,
    migrationUp,
    migrationDown,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export const setCustomRootFields = successCb => (dispatch, getState) => {
  const {
    allSchemas: allTables,
    currentTable: tableName,
    currentSchema: schemaName,
    modify: { rootFieldsEdit: newRootFields },
  } = getState().tables;

  dispatch({ type: SET_CUSTOM_ROOT_FIELDS });

  const tableDef = generateTableDef(tableName, schemaName);

  const table = findTable(allTables, tableDef);

  const existingRootFields = getTableCustomRootFields(table);
  const existingCustomColumnNames = getTableCustomColumnNames(table);

  const upQuery = getSetCustomRootFieldsQuery(
    tableDef,
    sanitiseRootFields(newRootFields),
    existingCustomColumnNames
  );
  const downQuery = getSetCustomRootFieldsQuery(
    tableDef,
    existingRootFields,
    existingCustomColumnNames
  );

  const migrationName = `set_custom_root_fields_${schemaName}_${tableName}`;
  const requestMsg = 'Setting custom root fields...';
  const successMsg = 'Setting custom root fields successful';
  const errorMsg = 'Setting custom root fields failed';
  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
  };
  const customOnError = err => {
    dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
  };

  makeMigrationCall(
    dispatch,
    getState,
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export const removeCheckConstraint = (constraintName, successCb, errorCb) => (
  dispatch,
  getState
) => {
  const confirmMessage = `This will permanently delete the check constraint "${constraintName}" from this table`;
  const isOk = getConfirmation(confirmMessage, true, constraintName);
  if (!isOk) return;

  const { currentTable: tableName, currentSchema } = getState().tables;

  const table = findTable(
    getState().tables.allSchemas,
    generateTableDef(tableName, currentSchema)
  );

  const constraint = findTableCheckConstraint(
    table.check_constraints,
    constraintName
  );

  const upQuery = getRunSqlQuery(
    dataSource.getDropConstraintSql(tableName, currentSchema, constraintName)
  );
  const downQuery = getRunSqlQuery(
    dataSource.getCreateCheckConstraintSql(
      tableName,
      currentSchema,
      constraintName,
      constraint.check
    )
  );

  const migrationName = `drop_check_constraint_${currentSchema}_${tableName}_${constraintName}`;
  const requestMsg = 'Deleting check constraint...';
  const successMsg = 'Check constraint deleted';
  const errorMsg = 'Deleting check constraint failed';
  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
  };
  const customOnError = err => {
    if (errorCb) {
      errorCb();
    }
    dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
  };
  makeMigrationCall(
    dispatch,
    getState,
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

const savePrimaryKeys = (tableName, schemaName, constraintName) => {
  return (dispatch, getState) => {
    dispatch({ type: SAVE_FOREIGN_KEY });
    // get selected configuration for PK
    const { pkModify } = getState().tables.modify;
    // table schema
    const tableSchema = getState().tables.allSchemas.find(
      ts => ts.table_name === tableName && ts.table_schema === schemaName
    );
    // form a list of selected PK columns
    let numSelectedPkColumns = 0;
    const selectedPkColumns = pkModify
      .filter(pk => pk !== '')
      .map(pk => {
        numSelectedPkColumns++;
        return tableSchema.columns[pk].column_name;
      });
    // form a list of existing PK columns
    const existingPkColumns = tableSchema.primary_key
      ? tableSchema.primary_key.columns
      : [];
    // compare list of existing PKs and newly selected PKs
    // TODO: Improve algorithm
    let changeDetected = false;
    if (selectedPkColumns.length === existingPkColumns.length) {
      for (let _i = selectedPkColumns.length - 1; _i >= 0; _i--) {
        if (selectedPkColumns[_i] !== existingPkColumns[_i]) {
          changeDetected = true;
          break;
        }
      }
    } else {
      changeDetected = true;
    }
    // Do nothing if no change is detected
    if (!changeDetected) {
      return dispatch(showSuccessNotification('No changes'));
    }

    const migrationUp = [];
    // skip dropping existing constraint if there is none
    if (constraintName) {
      migrationUp.push(
        getRunSqlQuery(
          dataSource.getDropConstraintSql(tableName, schemaName, constraintName)
        )
      );
    }
    // skip creating a new config if no columns were selected
    if (numSelectedPkColumns) {
      migrationUp.push(
        getRunSqlQuery(
          dataSource.getCreatePkSql({
            schemaName,
            tableName,
            selectedPkColumns,
            constraintName: `${tableName}_pkey`,
          })
        )
      );
    }

    const migrationDown = [];
    // skip dropping in down migration if no constraint was created
    if (numSelectedPkColumns) {
      migrationDown.push(
        getRunSqlQuery(
          dataSource.getDropConstraintSql(
            tableName,
            schemaName,
            `${tableName}_pkey`
          )
        )
      );
    }

    // skip creating in down migration if no constraint was dropped in up migration
    if (constraintName) {
      migrationDown.push(
        getRunSqlQuery(
          dataSource.getCreatePkSql({
            schemaName,
            tableName,
            selectedPkColumns: tableSchema.primary_key.columns,
            constraintName,
          })
        )
      );
    }

    const pkAction = numSelectedPkColumns ? 'Updating' : 'Deleting';
    const migrationName = `modify_primarykey_${schemaName}_${tableName}`;
    const requestMsg = `${pkAction} primary key constraint...`;
    const successMsg = `${pkAction} primary key constraint successful`;
    const errorMsg = `${pkAction} primary key constraint failed`;
    const customOnSuccess = () => {};
    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      migrationUp,
      migrationDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const saveForeignKeys = (index, tableSchema, columns) => {
  return (dispatch, getState) => {
    dispatch({ type: REMOVE_FOREIGN_KEY });
    const fk = getState().tables.modify.fkModify[index];
    const tableName = tableSchema.table_name;
    const schemaName = tableSchema.table_schema;
    const {
      refSchemaName,
      refTableName,
      colMappings,
      onUpdate,
      onDelete,
      constraintName,
    } = fk;
    const mappingObj = {};
    const filteredMappings = [];
    for (let _i = colMappings.length - 1; _i >= 0; _i--) {
      const cm = colMappings[_i];
      if (cm.column && cm.refColumn) {
        if (mappingObj[cm.column] !== undefined) {
          return dispatch(
            showErrorNotification(
              'Failed setting foreign key',
              `The column "${
                columns[cm.column].name
              }" seems to be referencing multiple foreign columns`
            )
          );
        }
        mappingObj[cm.column] = cm.refColumn;
        filteredMappings.push(cm);
      }
    }
    const lcols = filteredMappings.map(cm => `"${columns[cm.column].name}"`);
    const rcols = filteredMappings.map(cm => `"${cm.refColumn}"`);

    const migrationUp = [];
    const generatedConstraintName = generateFKConstraintName(
      tableName,
      lcols,
      tableSchema.foreign_key_constraints,
      [constraintName]
    );

    if (constraintName) {
      const migrationUpAlterFKeySql = dataSource.getAlterForeignKeySql(
        { schemaName, tableName, columns: lcols },
        {
          tableName: refTableName,
          schemaName: refSchemaName,
          columns: rcols,
        },
        constraintName,
        generatedConstraintName,
        onUpdate,
        onDelete
      );
      migrationUp.push(getRunSqlQuery(migrationUpAlterFKeySql));
    } else {
      const migrationUpCreateFKeySql = dataSource.getCreateFKeySql(
        {
          schemaName,
          tableName,
          columns: lcols,
        },
        {
          tableName: refTableName,
          schemaName: refSchemaName,
          columns: rcols,
        },
        generatedConstraintName,
        onUpdate,
        onDelete
      );
      migrationUp.push(getRunSqlQuery(migrationUpCreateFKeySql));
    }

    const migrationDown = [];

    if (constraintName) {
      // when foreign key is altered
      const oldConstraint = tableSchema.foreign_key_constraints[index];
      const migrationDownAlterFKeySql = dataSource.getAlterForeignKeySql(
        {
          schemaName,
          tableName,
          columns: Object.keys(oldConstraint.column_mapping).map(
            lc => `"${lc}"`
          ),
        },
        {
          schemaName: oldConstraint.ref_table_table_schema,
          tableName: oldConstraint.ref_table,
          columns: Object.values(oldConstraint.column_mapping).map(
            rc => `"${rc}"`
          ),
        },
        generatedConstraintName,
        constraintName,
        pgConfTypes[oldConstraint.on_update], // todo
        pgConfTypes[oldConstraint.on_delete] // todo
      );
      migrationDown.push(getRunSqlQuery(migrationDownAlterFKeySql));
    } else {
      // when foreign key is created
      const migrationDownDeleteFKeySql = dataSource.getDropConstraintSql(
        tableName,
        schemaName,
        generatedConstraintName
      );

      migrationDown.push(getRunSqlQuery(migrationDownDeleteFKeySql));
    }

    const migrationName = `set_fk_${schemaName}_${tableName}_${lcols.join(
      '_'
    )}`;
    const requestMsg = 'Saving foreign key...';
    const successMsg = 'Foreign key saved';
    const errorMsg = 'Failed setting foreign key';

    const customOnSuccess = () => {
      if (!constraintName) {
        const newFks = [...getState().tables.modify.fkModify];
        newFks[index].constraintName = generatedConstraintName;
        dispatch(
          setForeignKeys([
            ...newFks,
            {
              refTableName: '',
              colMappings: [{ column: '', refColumn: '' }],
              onUpdate: 'restrict',
              onDelete: 'restrict',
            },
          ])
        );
      } else {
        dispatch(
          setForeignKeys([
            ...getState().tables.modify.fkModify.slice(0, index),
            { ...fk },
            ...getState().tables.modify.fkModify.slice(index + 1),
          ])
        );
      }
    };
    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      migrationUp,
      migrationDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const removeForeignKey = (index, tableSchema) => {
  return (dispatch, getState) => {
    const tableName = tableSchema.table_name;
    const schemaName = tableSchema.table_schema;
    const oldConstraint = tableSchema.foreign_key_constraints[index];
    const upSql = dataSource.getDropConstraintSql(
      tableName,
      schemaName,
      oldConstraint.constraint_name
    );
    const downSql = dataSource.getCreateFKeySql(
      {
        schemaName,
        tableName,
        columns: Object.keys(oldConstraint.column_mapping).map(lc => `"${lc}"`),
      },
      {
        schemaName: oldConstraint.ref_table_table_schema,
        tableName: oldConstraint.ref_table,
        columns: Object.values(oldConstraint.column_mapping).map(
          rc => `"${rc}"`
        ),
      },
      oldConstraint.constraint_name,
      pgConfTypes[oldConstraint.on_update],
      pgConfTypes[oldConstraint.on_delete]
    );

    const migrationUp = [getRunSqlQuery(upSql)];
    const migrationDown = [getRunSqlQuery(downSql)];
    const migrationName = `delete_fk_${schemaName}_${tableName}_${oldConstraint.constraint_name}`;
    const requestMsg = 'Deleting foreign key...';
    const successMsg = 'Foreign key deleted';
    const errorMsg = 'Deleting foreign key failed';

    const customOnSuccess = () => {
      dispatch(
        setForeignKeys([
          ...getState().tables.modify.fkModify.slice(0, index),
          ...getState().tables.modify.fkModify.slice(index + 1),
        ])
      );
    };
    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      migrationUp,
      migrationDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const setUniqueKeys = keys => ({
  type: SET_UNIQUE_KEYS,
  keys,
});

const changeTableName = (oldName, newName, isTable, tableType) => {
  return (dispatch, getState) => {
    dispatch({ type: SAVE_NEW_TABLE_NAME });

    const property = tableType.toLowerCase();
    if (oldName === newName) {
      return dispatch(
        showErrorNotification(
          `Renaming ${property} failed`,
          `The ${property} name is already ${oldName}`
        )
      );
    }

    if (!gqlPattern.test(newName)) {
      const gqlValidationError = isTable
        ? gqlTableErrorNotif
        : gqlViewErrorNotif;
      return dispatch(
        showErrorNotification(
          gqlValidationError[3],
          gqlValidationError[1],
          gqlValidationError[2]
        )
      );
    }
    const compositeName = {
      TABLE: 'table',
      VIEW: 'view',
      'MATERIALIZED VIEW': 'materialized_view',
    }[tableType];
    const currentSchema = getState().tables.currentSchema;
    const upSql = dataSource.getRenameTableSql(
      property,
      currentSchema,
      oldName,
      newName
    );
    const downSql = dataSource.getRenameTableSql(
      property,
      currentSchema,
      newName,
      oldName
    );
    const migrateUp = [getRunSqlQuery(upSql)];
    const migrateDown = [getRunSqlQuery(downSql)];
    // apply migrations
    const migrationName =
      `rename_${compositeName}_` + currentSchema + '_' + oldName;

    const requestMsg = `Renaming ${property}...`;
    const successMsg = `Renaming ${property} successful`;
    const errorMsg = `Renaming ${property} failed`;

    const customOnSuccess = () => {
      dispatch(_push(getSchemaBaseRoute(currentSchema))); // to avoid 404
      dispatch(updateSchemaInfo()).then(() => {
        dispatch(_push(getTableModifyRoute(currentSchema, newName, isTable)));
      });
    };
    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
    };
    makeMigrationCall(
      dispatch,
      getState,
      migrateUp,
      migrateDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const deleteTrigger = (trigger, table) => {
  return (dispatch, getState) => {
    const triggerName = trigger.trigger_name;
    const triggerSchema = trigger.trigger_schema;

    const tableName = table.table_name;
    const tableSchema = table.table_schema;

    const upMigrationSql = dataSource.getDropTriggerSql(
      tableSchema,
      triggerName,
      tableName
    );
    const migrationUp = [getRunSqlQuery(upMigrationSql)];

    const downMigrationSql = dataSource.getCreateTriggerSql(
      tableName,
      tableSchema,
      triggerName,
      trigger
    );
    const migrationDown = [getRunSqlQuery(downMigrationSql)];

    const migrationName = `delete_trigger_${triggerSchema}_${triggerName}`;

    const requestMsg = 'Deleting trigger...';
    const successMsg = 'Trigger deleted';
    const errorMsg = 'Deleting trigger failed';

    const customOnSuccess = () => {};
    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      migrationUp,
      migrationDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const deleteTableSql = tableName => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    // handle no primary key
    const sqlDropTable = dataSource.getDropSql(tableName, currentSchema);
    const sqlUpQueries = [getRunSqlQuery(sqlDropTable)];
    // apply migrations
    const migrationName = 'drop_table_' + currentSchema + '_' + tableName;

    const requestMsg = 'Deleting table...';
    const successMsg = 'Table deleted';
    const errorMsg = 'Deleting table failed';

    const customOnSuccess = () => {
      dispatch(updateSchemaInfo());

      dispatch(_push('/data/'));
    };

    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      sqlUpQueries,
      [],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg,
      true
    );
  };
};

const untrackTableSql = tableName => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const tableDef = generateTableDef(tableName, currentSchema);
    const upQueries = [getUntrackTableQuery(tableDef)];
    const downQueries = [getTrackTableQuery(tableDef)];

    // apply migrations
    const migrationName = 'untrack_table_' + currentSchema + '_' + tableName;

    const requestMsg = 'Untracking table...';
    const successMsg = 'Table untracked';
    const errorMsg = 'Untrack table failed';

    const customOnSuccess = () => {
      dispatch(exportMetadata());
      dispatch(_push('/data/'));
    };
    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
    };

    makeMigrationCall(
      dispatch,
      getState,
      upQueries,
      downQueries,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg,
      true
    );
  };
};

const fetchViewDefinition = (viewName, isRedirect) => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const sqlQuery = dataSource.getViewDefinitionSql(viewName);
    const reqBody = getRunSqlQuery(sqlQuery, false, true);

    const url = Endpoints.query;
    const options = {
      method: 'POST',
      credentials: globalCookiePolicy,
      headers: dataHeaders(getState),
      body: JSON.stringify(reqBody),
    };
    return dispatch(
      requestAction(
        url,
        options,
        VIEW_DEF_REQUEST_SUCCESS,
        VIEW_DEF_REQUEST_ERROR
      )
    ).then(
      data => {
        const finalDef = data.result[1][0];
        // set state and redirect to run_sql
        if (isRedirect) {
          dispatch(_push('/data/sql'));
        }

        const viewType = data.result[1][1];
        const fullName = '"' + currentSchema + '"."' + viewName + '"';
        let runSqlDef = '';

        // todo, but can stay for now
        if (viewType == 'VIEW') {
          runSqlDef =
            'CREATE OR REPLACE VIEW ' + fullName + ' AS \n' + finalDef;
        } else {
          runSqlDef =
            'DROP MATERIALIZED VIEW ' +
            fullName +
            ';\n' +
            'CREATE MATERIALIZED VIEW ' +
            fullName +
            ' AS \n' +
            finalDef;
        }
        dispatch({ type: SET_VIEW_DEF_SQL, data: runSqlDef });
      },
      err => {
        dispatch(
          showErrorNotification('Fetching definition failed!', err.error, err)
        );
      }
    );
  };
};

const deleteViewSql = (viewName, viewType) => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const property = viewType.toLowerCase();
    const sqlDropView = dataSource.getDropSql(
      viewName,
      currentSchema,
      viewType
    );
    const sqlUpQueries = [getRunSqlQuery(sqlDropView)];

    // Apply migrations
    const migrationName = 'drop_view_' + currentSchema + '_' + viewName;

    const requestMsg = `Deleting ${property}...`;
    const successMsg = `${capitalize(property)} deleted`;
    const errorMsg = `Deleting ${property} failed`;

    const customOnSuccess = () => {
      dispatch(_push('/data/'));
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      sqlUpQueries,
      [],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const deleteColumnSql = (column, tableSchema) => {
  return (dispatch, getState) => {
    const name = column.column_name;
    const tableName = column.table_name;
    const currentSchema = column.table_schema;
    const comment = column.comment;
    const is_nullable = column.is_nullable;
    const col_type = column.udt_name;
    const foreign_key_constraints = tableSchema.foreign_key_constraints.filter(
      fkc => {
        const columnKeys = Object.keys(fkc.column_mapping);
        return columnKeys.includes(name);
      }
    );
    const opp_foreign_key_constraints = tableSchema.opp_foreign_key_constraints.filter(
      fkc => {
        const columnKeys = Object.values(fkc.column_mapping);
        return columnKeys.includes(name);
      }
    );
    const unique_constraints = tableSchema.unique_constraints.filter(uc =>
      uc.columns.includes(name)
    );

    const schemaChangesUp = [
      getRunSqlQuery(
        dataSource.getDropColumnSql(tableName, currentSchema, name)
      ),
    ];

    const schemaChangesDown = [];
    schemaChangesDown.push(
      getRunSqlQuery(
        dataSource.getAddColumnSql(tableName, currentSchema, name, col_type)
      )
    );

    if (is_nullable) {
      schemaChangesDown.push(
        getRunSqlQuery(
          dataSource.getDropNotNullSql(tableName, currentSchema, name)
        )
      );
    } else {
      schemaChangesDown.push(
        getRunSqlQuery(
          dataSource.getSetNotNullSql(tableName, currentSchema, name)
        )
      );
    }

    const merged_fkc = foreign_key_constraints.concat(
      opp_foreign_key_constraints
    );
    if (merged_fkc.length > 0) {
      merged_fkc.forEach(fkc => {
        // add foreign key constraint to down migration
        const lcol = Object.keys(fkc.column_mapping);
        const rcol = Object.values(fkc.column_mapping);
        const onUpdate = pgConfTypes[fkc.on_update];
        const onDelete = pgConfTypes[fkc.on_delete];
        schemaChangesDown.push(
          getRunSqlQuery(
            dataSource.getCreateFKeySql(
              { tableName, schemaName: currentSchema, columns: lcol },
              {
                tableName: fkc.ref_table,
                schemaName: fkc.ref_table_table_schema,
                columns: rcol,
              },
              fkc.constraint_name,
              onUpdate,
              onDelete
            )
          )
        );
      });
    }

    if (unique_constraints.length > 0) {
      unique_constraints.forEach(uc => {
        // add unique constraint to down migration
        schemaChangesDown.push(
          getRunSqlQuery(
            dataSource.getAddUniqueConstraintSql(
              tableName,
              currentSchema,
              uc.constraint_name,
              uc.columns
            )
          )
        );
      });
    }

    if (column.column_default !== null) {
      schemaChangesDown.push(
        getRunSqlQuery(
          dataSource.getSetColumnDefaultSql(
            tableName,
            currentSchema,
            name,
            column.column_default,
            col_type
          )
        )
      );
    }

    if (comment) {
      schemaChangesDown.push(
        getRunSqlQuery(
          dataSource.getSetCommentSql(
            'column',
            tableName,
            currentSchema,
            name,
            comment,
            col_type
          )
        )
      );
    }

    // Apply migrations
    const migrationName =
      'alter_table_' + currentSchema + '_' + tableName + '_drop_column_' + name;

    const requestMsg = 'Deleting Column...';
    const successMsg = 'Column deleted';
    const errorMsg = 'Deleting column failed';

    const customOnSuccess = (data, consoleMode, migrationMode) => {
      if (consoleMode === CLI_CONSOLE_MODE && migrationMode) {
        // show warning information
        dispatch(
          showWarningNotification(
            'Check down migration',
            'Please verify that the down migration will reset the DB to the previous state (you ' +
              'might need to add recreation of some dependent objects like indexes, etc.)',
            data
          )
        );
      }
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      schemaChangesUp,
      schemaChangesDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const addColSql = (
  tableName,
  colName,
  colType,
  colNull,
  colUnique,
  colDefault,
  colDependentSQLGenerator,
  callback
) => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const sqlUp = dataSource.getAddColumnSql(
      tableName,
      currentSchema,
      colName,
      colType,
      {
        nullable: colNull,
        unique: colUnique,
        default: colDefault,
        sqlGenerator: colDependentSQLGenerator,
      }
    );

    const schemaChangesUp = [];
    if (Array.isArray(sqlUp)) {
      sqlUp.forEach(sql => schemaChangesUp.push(getRunSqlQuery(sql)));
    } else {
      schemaChangesUp.push(getRunSqlQuery(sqlUp));
    }

    const runSqlQueryDown = dataSource.getDropColumnSql(
      tableName,
      currentSchema,
      colName,
      colDependentSQLGenerator
    );

    const schemaChangesDown = [getRunSqlQuery(runSqlQueryDown)];

    const migrationName =
      'alter_table_' +
      currentSchema +
      '_' +
      tableName +
      '_add_column_' +
      colName;

    const requestMsg = 'Adding Column...';
    const successMsg = 'Column added';
    const errorMsg = 'Adding column failed';

    const customOnSuccess = () => {
      callback();
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      schemaChangesUp,
      schemaChangesDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const activateCommentEdit = (isEnabled, value) => ({
  type: TABLE_COMMENT_EDIT,
  data: { enabled: isEnabled, value: value },
});
const updateCommentInput = value => ({
  type: TABLE_COMMENT_INPUT_EDIT,
  value: value,
});

const deleteConstraintSql = (tableName, cName) => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const schemaChangesUp = [
      getRunSqlQuery(
        dataSource.getDropConstraintSql(tableName, currentSchema, cName)
      ),
    ];

    const migrationName =
      'alter_table_' + currentSchema + '_' + tableName + '_drop_foreign_key';

    const requestMsg = 'Deleting Constraint...';
    const successMsg = 'Constraint deleted';
    const errorMsg = 'Deleting constraint failed';

    const customOnSuccess = () => {};
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      schemaChangesUp,
      [],
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

const saveTableCommentSql = tableType => {
  return (dispatch, getState) => {
    const updatedComment = getState().tables.modify.tableCommentEdit
      .editedValue;

    const currentSchema = getState().tables.currentSchema;
    const tableName = getState().tables.currentTable;

    const commentQueryUp = dataSource.getSetCommentSql(
      tableType,
      tableName,
      currentSchema,
      updatedComment || null
    );

    const commentDownQuery = dataSource.getSetCommentSql(
      tableType,
      tableName,
      currentSchema,
      'NULL'
    );
    const schemaChangesUp = [getRunSqlQuery(commentQueryUp)];
    const schemaChangesDown = [getRunSqlQuery(commentDownQuery)];

    // Apply migrations
    const migrationName =
      'alter_table_' + currentSchema + '_' + tableName + '_update_comment';

    const requestMsg = 'Updating Comment...';
    const successMsg = 'Comment Updated';
    const errorMsg = 'Updating comment failed';

    const customOnSuccess = () => {
      // Instead of calling loadSchema, update only the table comment in the state.
      // get existing state and filter out with table name and table schema.
      // update the comment and set in the state.
      const existingSchemas = getState().tables.allSchemas.filter(
        schemaInfo =>
          !(
            schemaInfo.table_name !== tableName &&
            schemaInfo.table_schema !== currentSchema
          )
      );
      const currentSchemaInfo = getState().tables.allSchemas.find(
        schemaInfo =>
          schemaInfo.table_name === tableName &&
          schemaInfo.table_schema === currentSchema
      );
      currentSchemaInfo.comment = updatedComment;
      dispatch({
        type: LOAD_SCHEMA,
        allSchemas: existingSchemas.concat(currentSchemaInfo),
      });
      dispatch(activateCommentEdit(false, null));
    };
    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      schemaChangesUp,
      schemaChangesDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg,
      true
    );
  };
};

const isColumnUnique = (tableSchema, colName) => {
  return (
    tableSchema.unique_constraints.filter(
      constraint =>
        constraint.columns.includes(colName) && constraint.columns.length === 1
    ).length > 0
  );
};

const saveColumnChangesSql = (colName, column, onSuccess) => {
  return (dispatch, getState) => {
    const columnEdit = getState().tables.modify.columnEdit[colName];

    const { tableName } = columnEdit;
    const colType = columnEdit.type;
    const nullable = columnEdit.isNullable;
    const unique = columnEdit.isUnique;
    const colDefault = (columnEdit.default || '').trim();
    const comment = (columnEdit.comment || '').trim();
    const newName = columnEdit.name.trim();
    const currentSchema = columnEdit.schemaName;
    const customFieldName = (columnEdit.customFieldName || '').trim();

    const tableDef = generateTableDef(tableName, currentSchema);
    const table = findTable(getState().tables.allSchemas, tableDef);

    // check if column type has changed before making it part of the migration
    const originalColType = column.udt_name; // "value"
    const originalColDefault = column.column_default || ''; // null or "value"
    const originalColComment = column.comment || ''; // null or "value"
    const originalColNullable = column.is_nullable; // "YES" or "NO"
    const originalColUnique = isColumnUnique(table, colName);

    const columnChangesUpQuery = dataSource.getAlterColumnTypeSql(
      tableName,
      currentSchema,
      colName,
      colType
    );
    const columnChangesDownQuery = dataSource.getAlterColumnTypeSql(
      tableName,
      currentSchema,
      colName,
      column.data_type
    );

    const schemaChangesUp =
      originalColType !== colType ? [getRunSqlQuery(columnChangesUpQuery)] : [];
    let schemaChangesDown =
      originalColType !== colType
        ? [getRunSqlQuery(columnChangesDownQuery)]
        : [];

    /* column custom field up/down migration*/
    const existingCustomColumnNames = getTableCustomColumnNames(table);
    const existingRootFields = getTableCustomRootFields(table);
    const newCustomColumnNames = { ...existingCustomColumnNames };
    let isCustomFieldNameChanged = false;
    if (customFieldName) {
      if (customFieldName !== existingCustomColumnNames[colName]) {
        isCustomFieldNameChanged = true;
        newCustomColumnNames[colName] = customFieldName.trim();
      }
    } else {
      if (existingCustomColumnNames[colName]) {
        isCustomFieldNameChanged = true;
        delete newCustomColumnNames[colName];
      }
    }
    if (isCustomFieldNameChanged) {
      schemaChangesUp.push(
        getSetCustomRootFieldsQuery(
          tableDef,
          existingRootFields,
          newCustomColumnNames
        )
      );
      schemaChangesDown.push(
        getSetCustomRootFieldsQuery(
          tableDef,
          existingRootFields,
          existingCustomColumnNames
        )
      );
    }

    /* column default up/down migration */
    let columnDefaultUpQuery;
    if (colDefault !== '') {
      columnDefaultUpQuery = dataSource.getSetColumnDefaultSql(
        tableName,
        currentSchema,
        colName,
        colDefault
      );
    } else {
      columnDefaultUpQuery = dataSource.getDropColumnDefaultSql(
        tableName,
        currentSchema,
        colName
      );
    }

    let columnDefaultDownQuery;
    if (originalColDefault !== '') {
      columnDefaultDownQuery = dataSource.getSetColumnDefaultSql(
        tableName,
        currentSchema,
        colName,
        originalColDefault
      );
    } else {
      columnDefaultDownQuery = dataSource.getDropColumnDefaultSql(
        tableName,
        currentSchema,
        colName
      );
    }

    // check if default is unchanged and then do a drop. if not skip
    if (originalColDefault !== colDefault) {
      schemaChangesUp.push(getRunSqlQuery(columnDefaultUpQuery));
      schemaChangesDown.push(getRunSqlQuery(columnDefaultDownQuery));
    }

    /* column nullable up/down migration */
    if (nullable) {
      const nullableUpQuery = dataSource.getDropNotNullSql(
        tableName,
        currentSchema,
        colName
      );
      const nullableDownQuery = dataSource.getSetNotNullSql(
        tableName,
        currentSchema,
        colName
      );
      // check with original null
      if (originalColNullable !== 'YES') {
        schemaChangesUp.push(getRunSqlQuery(nullableUpQuery));
        schemaChangesDown.push(getRunSqlQuery(nullableDownQuery));
      }
    } else {
      const nullableUpQuery = dataSource.getSetNotNullSql(
        tableName,
        currentSchema,
        colName
      );
      const nullableDownQuery = dataSource.getDropNotNullSql(
        tableName,
        currentSchema,
        colName
      );
      // check with original null
      if (originalColNullable !== 'NO') {
        schemaChangesUp.push(getRunSqlQuery(nullableUpQuery));
        schemaChangesDown.push(getRunSqlQuery(nullableDownQuery));
      }
    }

    /* column unique up/down migration */
    if (unique) {
      const uniqueUpQuery = dataSource.getAddUniqueConstraintSql(
        tableName,
        currentSchema,
        tableName + '_' + colName + '_key',
        [colName]
      );
      const uniqueDownQuery = dataSource.getDropConstraintSql(
        tableName,
        currentSchema,
        tableName + '_' + colName + '_key'
      );
      // check with original unique
      if (!originalColUnique) {
        schemaChangesUp.push(getRunSqlQuery(uniqueUpQuery));
        schemaChangesDown.push(getRunSqlQuery(uniqueDownQuery));
      }
    } else {
      const uniqueDownQuery = dataSource.getAddUniqueConstraintSql(
        tableName,
        currentSchema,
        tableName + '_' + colName + '_key',
        [colName]
      );
      const uniqueUpQuery = dataSource.getDropConstraintSql(
        tableName,
        currentSchema,
        tableName + '_' + colName + '_key'
      );
      // check with original unique
      if (originalColUnique) {
        schemaChangesUp.push(getRunSqlQuery(uniqueUpQuery));
        schemaChangesDown.push(getRunSqlQuery(uniqueDownQuery));
      }
    }

    /* column comment up/down migration */
    if (originalColComment !== comment) {
      const columnCommentUpQuery = dataSource.getSetCommentSql(
        'column',
        tableName,
        currentSchema,
        colName,
        comment,
        colType
      );

      const columnCommentDownQuery = dataSource.getSetCommentSql(
        'column',
        tableName,
        currentSchema,
        colName,
        originalColComment,
        colType
      );

      schemaChangesUp.push(getRunSqlQuery(columnCommentUpQuery));
      schemaChangesDown.push(getRunSqlQuery(columnCommentDownQuery));
    }

    /* rename column */
    if (newName && colName !== newName) {
      if (!gqlPattern.test(newName)) {
        return dispatch(
          showErrorNotification(
            gqlColumnErrorNotif[3],
            gqlColumnErrorNotif[1],
            gqlColumnErrorNotif[2]
          )
        );
      }
      schemaChangesUp.push(
        getRunSqlQuery(
          dataSource.getRenameColumnQuery(
            tableName,
            currentSchema,
            newName,
            colName,
            colType
          )
        )
      );
      schemaChangesDown = [
        getRunSqlQuery(
          dataSource.getRenameColumnQuery(
            tableName,
            currentSchema,
            colName,
            newName,
            colType
          )
        ),
        ...schemaChangesDown,
      ];
    }

    // Apply migrations
    const migrationName =
      'alter_table_' +
      currentSchema +
      '_' +
      tableName +
      '_alter_column_' +
      colName;

    const requestMsg = 'Saving Column Changes...';
    const successMsg = 'Column modified';
    const errorMsg = 'Modifying column failed';

    const customOnSuccess = () => {
      dispatch(resetColumnEdit(colName));
      onSuccess();
    };
    const customOnError = () => {};

    if (schemaChangesUp.length > 0) {
      makeMigrationCall(
        dispatch,
        getState,
        schemaChangesUp,
        schemaChangesDown,
        migrationName,
        customOnSuccess,
        customOnError,
        requestMsg,
        successMsg,
        errorMsg
      );
    } else {
      dispatch(
        showSuccessNotification(
          'Nothing to modify. No changes to column was detected.'
        )
      );
    }
  };
};

const fetchColumnCasts = () => {
  return (dispatch, getState) => {
    const url = Endpoints.getSchema;
    // todo -- it should be normalized for different data sources
    const reqQuery = getRunSqlQuery(dataSource.fetchColumnCastsQuery);
    const options = {
      credentials: globalCookiePolicy,
      method: 'POST',
      headers: dataHeaders(getState),
      body: JSON.stringify(reqQuery),
    };
    return dispatch(requestAction(url, options)).then(
      data => {
        return dispatch({
          type: FETCH_COLUMN_TYPE_CASTS,
          // todo: dataSource.convertTypeCasts or something like that
          data: convertArrayToJson(data.result.slice(1)),
        });
      },
      error => {
        dispatch(
          showErrorNotification(
            'Error fetching column casts information',
            'Kindly reach out to us in case you face this issue again',
            error
          )
        );
        return dispatch({
          type: FETCH_COLUMN_TYPE_CASTS_FAIL,
          data: error,
        });
      }
    );
  };
};

const removeUniqueKey = (index, tableName, existingConstraints, callback) => {
  return (dispatch, getState) => {
    dispatch({ type: REMOVE_UNIQUE_KEY });
    const { currentSchema } = getState().tables;
    const existingConstraint = existingConstraints[index];

    // Up migration: Drop the constraint
    const sqlUp = [
      getRunSqlQuery(
        dataSource.getDropConstraintSql(
          tableName,
          currentSchema,
          existingConstraint.constraint_name
        )
      ),
    ];

    // Down Migration: Create the constraint that is being dropped
    const sqlDown = [
      getRunSqlQuery(
        dataSource.getAddUniqueConstraintSql(
          tableName,
          currentSchema,
          getUniqueConstraintName(tableName, existingConstraint.columns),
          existingConstraint.columns.map(c => `"${c}"`)
        )
      ),
    ];

    const migrationName =
      'alter_table_' +
      currentSchema +
      '_' +
      tableName +
      '_drop_constraint_' +
      existingConstraint.constraint_name;

    const requestMsg = 'Deleting Constraint...';
    const successMsg = 'Constraint deleted';
    const errorMsg = 'Deleting constraint failed';

    const customOnSuccess = () => {
      if (callback) {
        callback();
      }

      // remove the removed unique constraint from state
      const uniqueKeysInState = getState().tables.modify.uniqueKeyModify;
      dispatch(
        setUniqueKeys([
          ...uniqueKeysInState.slice(0, index),
          ...uniqueKeysInState.slice(index + 1),
        ])
      );
    };

    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      sqlUp,
      sqlDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const toggleTableAsEnum = (isEnum, successCallback, failureCallback) => (
  dispatch,
  getState
) => {
  const confirmMessage = `This will ${
    isEnum ? 'un' : ''
  }set this table as an enum`;
  const isOk = getConfirmation(confirmMessage);
  if (!isOk) {
    return;
  }

  dispatch({ type: TOGGLE_ENUM });

  const { currentTable, currentSchema } = getState().tables;
  const { allSchemas } = getState().tables;

  const upQuery = [
    getSetTableEnumQuery(
      generateTableDef(currentTable, currentSchema),
      !isEnum
    ),
  ];
  const downQuery = [
    getSetTableEnumQuery(generateTableDef(currentTable, currentSchema), isEnum),
  ];

  const migrationName =
    'alter_table_' +
    currentSchema +
    '_' +
    currentTable +
    '_set_enum_' +
    !isEnum;

  const action = !isEnum ? 'Setting' : 'Unsetting';

  const requestMsg = `${action} table as enum...`;
  const successMsg = `${action} table as enum successful`;
  const errorMsg = `${action} table as enum failed`;

  const customOnSuccess = () => {
    if (successCallback) {
      successCallback();
    }

    dispatch(toggleEnumSuccess());

    const newAllSchemas = allSchemas.map(schema => {
      if (
        schema.table_name === currentTable &&
        schema.table_schema === currentSchema
      ) {
        return {
          ...schema,
          is_enum: !isEnum,
        };
      }
      return schema;
    });

    dispatch({ type: LOAD_SCHEMA, allSchemas: newAllSchemas });
  };

  const customOnError = () => {
    dispatch(toggleEnumFailure());

    if (failureCallback) {
      failureCallback();
    }
  };

  makeMigrationCall(
    dispatch,
    getState,
    upQuery,
    downQuery,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export const saveCheckConstraint = (index, successCb, errorCb) => (
  dispatch,
  getState
) => {
  const {
    currentTable,
    currentSchema,
    allSchemas: allTables,
  } = getState().tables;
  const { checkConstraintsModify } = getState().tables.modify;

  const allConstraints = findTable(
    allTables,
    generateTableDef(currentTable, currentSchema)
  ).check_constraints;

  const newConstraint = checkConstraintsModify[index];

  const isNew = index === allConstraints.length;

  const existingConstraint = allConstraints[index];

  const upQueries = [];
  const downQueries = [];

  if (!isNew) {
    upQueries.push(
      getRunSqlQuery(
        dataSource.getDropConstraintSql(
          existingConstraint.table_name,
          existingConstraint.table_schema,
          existingConstraint.constraint_name
        )
      )
    );
  }
  upQueries.push(
    getRunSqlQuery(
      dataSource.getCreateCheckConstraintSql(
        currentTable,
        currentSchema,
        newConstraint.name,
        newConstraint.check
      )
    )
  );

  downQueries.push(
    getRunSqlQuery(
      dataSource.getDropConstraintSql(
        currentTable,
        currentSchema,
        newConstraint.name
      )
    )
  );

  if (!isNew) {
    downQueries.push(
      getRunSqlQuery(
        dataSource.getCreateCheckConstraintSql(
          currentTable,
          currentSchema,
          existingConstraint.constraint_name,
          existingConstraint.check
        )
      )
    );
  }

  const migrationName =
    'alter_table_' +
    currentSchema +
    '_' +
    currentTable +
    '_add_check_constraint_' +
    newConstraint.name;
  const requestMsg = 'Saving check constraint...';
  const successMsg = 'Check constraint saved';
  const errorMsg = 'Saving check constraint failed';

  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
  };

  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  makeMigrationCall(
    dispatch,
    getState,
    upQueries,
    downQueries,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

const saveUniqueKey = (
  index,
  tableName,
  allColumns,
  existingConstraints,
  callback
) => {
  return (dispatch, getState) => {
    dispatch({ type: SAVE_UNIQUE_KEY });
    const { currentSchema } = getState().tables;
    const uniqueKeys = getState().tables.modify.uniqueKeyModify;
    const numUniqueKeys = uniqueKeys.length;
    const uniqueKey = uniqueKeys[index];
    const columns = uniqueKey.map(c => allColumns[c].name);
    const existingConstraint = existingConstraints[index];

    const downMigration = [];
    // drop the newly created constraint
    downMigration.push(
      getRunSqlQuery(
        dataSource.getDropConstraintSql(
          tableName,
          currentSchema,
          getUniqueConstraintName(tableName, columns)
        )
      )
    );
    // if any constraint is being dropped, create it back
    if (index < numUniqueKeys - 1) {
      downMigration.push(
        getRunSqlQuery(
          dataSource.getAddUniqueConstraintSql(
            tableName,
            currentSchema,
            getUniqueConstraintName(tableName, existingConstraint.columns),
            existingConstraint.columns.map(c => `"${c}"`)
          )
        )
      );
    }

    const upMigration = [];
    // drop the old constraint if there is any
    if (index < numUniqueKeys - 1) {
      upMigration.push(
        getRunSqlQuery(
          dataSource.getDropConstraintSql(
            tableName,
            currentSchema,
            existingConstraint.constraint_name
          )
        )
      );
    }

    // create the new constraint
    upMigration.push(
      getRunSqlQuery(
        dataSource.getAddUniqueConstraintSql(
          tableName,
          currentSchema,
          getUniqueConstraintName(tableName, columns),
          columns.map(c => `"${c}"`)
        )
      )
    );

    const migrationName =
      'alter_table_' +
      currentSchema +
      '_' +
      tableName +
      '_add_unique_' +
      columns.join('_');
    const requestMsg = 'Saving unique key...';
    const successMsg = 'Unique key saved';
    const errorMsg = 'Saving unique key failed';

    const customOnSuccess = () => {
      // success callback
      if (callback) {
        callback();
      }

      // add an empty unique key to state
      const uniqueKeysInState = getState().tables.modify.uniqueKeyModify;
      dispatch(setUniqueKeys([...uniqueKeysInState, []]));
    };

    const customOnError = () => {};

    makeMigrationCall(
      dispatch,
      getState,
      upMigration,
      downMigration,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  };
};

export const setViewCustomColumnNames = (
  customColumnNames,
  viewName,
  schemaName,
  successCb,
  errorCb
) => (dispatch, getState) => {
  const viewDef = generateTableDef(viewName, schemaName);
  const view = findTable(getState().tables.allSchemas, viewDef);

  const existingCustomRootFields = getTableCustomRootFields(view);
  const existingColumnNames = getTableCustomColumnNames(view);

  const upQuery = getSetCustomRootFieldsQuery(
    viewDef,
    existingCustomRootFields,
    sanitiseColumnNames(customColumnNames)
  );
  const downQuery = getSetCustomRootFieldsQuery(
    viewDef,
    existingCustomRootFields,
    existingColumnNames
  );

  const migrationName = 'alter_view_custom_column_names';
  const requestMsg = 'Saving column metadata...';
  const successMsg = 'Saved column metadata successfully';
  const errorMsg = 'Saving column metadata failed';

  const customOnSuccess = () => {
    if (successCb) {
      successCb();
    }
  };

  const customOnError = () => {
    if (errorCb) {
      errorCb();
    }
  };

  makeMigrationCall(
    dispatch,
    getState,
    [upQuery],
    [downQuery],
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export {
  FETCH_COLUMN_TYPE_CASTS,
  FETCH_COLUMN_TYPE_CASTS_FAIL,
  VIEW_DEF_REQUEST_SUCCESS,
  VIEW_DEF_REQUEST_ERROR,
  SET_VIEW_DEF_SQL,
  SET_COLUMN_EDIT,
  TABLE_COMMENT_EDIT,
  TABLE_COMMENT_INPUT_EDIT,
  SAVE_NEW_TABLE_NAME,
  EDIT_COLUMN,
  RESET_COLUMN_EDIT,
  ADD_PRIMARY_KEY,
  REMOVE_PRIMARY_KEY,
  RESET_PRIMARY_KEY,
  SET_PRIMARY_KEYS,
  DELETE_PK_WARNING,
  SET_FOREIGN_KEYS,
  RESET,
  SET_UNIQUE_KEYS,
  TOGGLE_ENUM,
  TOGGLE_ENUM_SUCCESS,
  TOGGLE_ENUM_FAILURE,
  MODIFY_ROOT_FIELD,
  SET_CHECK_CONSTRAINTS,
  changeTableName,
  fetchViewDefinition,
  handleMigrationErrors,
  saveColumnChangesSql,
  addColSql,
  deleteColumnSql,
  setColumnEdit,
  resetColumnEdit,
  deleteConstraintSql,
  deleteTableSql,
  untrackTableSql,
  deleteViewSql,
  isColumnUnique,
  activateCommentEdit,
  updateCommentInput,
  saveTableCommentSql,
  editColumn,
  addPrimaryKey,
  removePrimaryKey,
  resetPrimaryKeys,
  setPrimaryKeys,
  savePrimaryKeys,
  setForeignKeys,
  saveForeignKeys,
  removeForeignKey,
  fetchColumnCasts,
  setUniqueKeys,
  removeUniqueKey,
  saveUniqueKey,
  deleteTrigger,
  toggleEnumSuccess,
  toggleEnumFailure,
  modifyRootFields,
  setCheckConstraints,
};
