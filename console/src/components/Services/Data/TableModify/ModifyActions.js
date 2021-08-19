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
  getTableCustomName,
  generateTableDef,
  dataSource,
  escapeTableColumns,
  escapeTableName,
  currentDriver,
} from '../../../../dataSources';
import { getRunSqlQuery } from '../../../Common/utils/v1QueryUtils';
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
import {
  getTrackTableQuery,
  getUntrackTableQuery,
  getAddComputedFieldQuery,
  getDropComputedFieldQuery,
  getSetCustomRootFieldsQuery,
  getSetTableEnumQuery,
} from '../../../../metadata/queryUtils';
import { getColumnUpdateMigration } from '../../../../utils/migration/utils';
import Migration from '../../../../utils/migration/Migration';
import { setSidebarLoading } from '../DataSubSidebar';

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

const MODIFY_TABLE_CUSTOM_NAME = 'ModifyTable/MODIFY_TABLE_CUSTOM_NAME';
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

const modifyTableCustomName = customName => ({
  type: MODIFY_TABLE_CUSTOM_NAME,
  data: customName,
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
  const { currentDataSource } = getState().tables;

  const tableDef = getTableDef(table);

  const computedFieldName = computedField.computed_field_name;

  if (originalComputedField) {
    migrationUp.push(
      getDropComputedFieldQuery(
        tableDef,
        originalComputedField.computed_field_name,
        currentDataSource
      )
    );
  }

  migrationUp.push(
    getAddComputedFieldQuery(
      tableDef,
      computedFieldName,
      computedField.definition,
      computedField.comment,
      currentDataSource
    )
  );

  migrationDown.push(
    getDropComputedFieldQuery(tableDef, computedFieldName, currentDataSource)
  );

  if (originalComputedField) {
    migrationDown.push(
      getAddComputedFieldQuery(
        tableDef,
        originalComputedField.computed_field_name,
        originalComputedField.definition,
        originalComputedField.comment,
        currentDataSource
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
  const { currentDataSource } = getState().tables;

  const tableDef = getTableDef(table);
  const computedFieldName = computedField.computed_field_name;

  const migration = new Migration();
  migration.add(
    getDropComputedFieldQuery(tableDef, computedFieldName, currentDataSource),
    getAddComputedFieldQuery(
      tableDef,
      computedFieldName,
      computedFieldName.definition,
      computedField.comment,
      currentDataSource
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
    migration.upMigration,
    migration.downMigration,
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
    modify: { rootFieldsEdit: newRootFields, custom_name: customName },
  } = getState().tables;
  const { currentDataSource } = getState().tables;

  dispatch({ type: SET_CUSTOM_ROOT_FIELDS });

  const tableDef = generateTableDef(tableName, schemaName);

  const table = findTable(allTables, tableDef);

  const existingCustomName = getTableCustomName(table);
  const existingRootFields = getTableCustomRootFields(table);
  const existingCustomColumnNames = getTableCustomColumnNames(table);
  const migration = new Migration();

  migration.add(
    getSetCustomRootFieldsQuery(
      tableDef,
      sanitiseRootFields(newRootFields),
      existingCustomColumnNames,
      customName,
      currentDataSource
    ),
    getSetCustomRootFieldsQuery(
      tableDef,
      existingRootFields,
      existingCustomColumnNames,
      existingCustomName,
      currentDataSource
    )
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
    migration.upMigration,
    migration.downMigration,
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

  const {
    currentTable: tableName,
    currentSchema,
    currentDataSource,
  } = getState().tables;

  const table = findTable(
    getState().tables.allSchemas,
    generateTableDef(tableName, currentSchema)
  );

  const constraint = findTableCheckConstraint(
    table.check_constraints,
    constraintName
  );
  const migration = new Migration();
  migration.add(
    getRunSqlQuery(
      dataSource.getDropConstraintSql(tableName, currentSchema, constraintName),
      currentDataSource
    ),
    getRunSqlQuery(
      dataSource.getCreateCheckConstraintSql(
        tableName,
        currentSchema,
        constraintName,
        constraint.check
      ),
      currentDataSource
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
    migration.upMigration,
    migration.downMigration,
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
    const source = getState().tables.currentDataSource;
    const { pkModify } = getState().tables.modify;
    const tableSchema = getState().tables.allSchemas.find(
      ts => ts.table_name === tableName && ts.table_schema === schemaName
    );
    let numSelectedPkColumns = 0;
    const selectedPkColumns = pkModify
      .filter(pk => pk !== '')
      .map(pk => {
        numSelectedPkColumns++;
        return tableSchema.columns[pk].column_name;
      });
    const existingPkColumns = tableSchema.primary_key
      ? tableSchema.primary_key.columns
      : [];
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
    if (!changeDetected) {
      return dispatch(showSuccessNotification('No changes'));
    }
    const migrationUp = [];
    // droping PK
    if (constraintName && !numSelectedPkColumns) {
      migrationUp.push(
        getRunSqlQuery(
          dataSource.getDropConstraintSql(
            tableName,
            schemaName,
            constraintName
          ),
          source
        )
      );
    }
    // Altering PK
    else if (constraintName && numSelectedPkColumns) {
      migrationUp.push(
        getRunSqlQuery(
          dataSource.getAlterPkSql({
            schemaName,
            tableName,
            selectedPkColumns,
            constraintName,
          }),
          source
        )
      );
    }
    // Creating a new PK entry
    else if (!constraintName && numSelectedPkColumns) {
      migrationUp.push(
        getRunSqlQuery(
          dataSource.getCreatePkSql({
            schemaName,
            tableName,
            selectedPkColumns,
            constraintName: `${tableName}_pkey`,
          }),
          source
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
          ),
          source
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
          }),
          source
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
    const source = getState().tables.currentDataSource;
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

    const generatedConstraintName = generateFKConstraintName(
      tableName,
      lcols,
      tableSchema.foreign_key_constraints,
      [constraintName]
    );
    const migrationUp = [];
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
      migrationUp.push(getRunSqlQuery(migrationUpAlterFKeySql, source));
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
      migrationUp.push(getRunSqlQuery(migrationUpCreateFKeySql, source));
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
        dataSource.getReferenceOption(oldConstraint.on_update),
        dataSource.getReferenceOption(oldConstraint.on_delete)
      );
      migrationDown.push(getRunSqlQuery(migrationDownAlterFKeySql, source));
    } else {
      // when foreign key is created
      const migrationDownDeleteFKeySql = dataSource.getDropConstraintSql(
        tableName,
        schemaName,
        generatedConstraintName
      );

      migrationDown.push(getRunSqlQuery(migrationDownDeleteFKeySql, source));
    }

    const migrationName = `set_fk_${schemaName}_${tableName}_${lcols.join(
      '_'
    )}`;
    const requestMsg = 'Saving foreign key...';
    const successMsg = 'Foreign key saved';
    const errorMsg = 'Failed setting foreign key';
    const violiationActions = dataSource?.violationActions;
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
              onUpdate: violiationActions?.[0],
              onDelete: violiationActions?.[0],
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
    const source = getState().tables.currentDataSource;
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
      dataSource.getReferenceOption(oldConstraint.on_update),
      dataSource.getReferenceOption(oldConstraint.on_delete)
    );

    const migrationUp = [getRunSqlQuery(upSql, source)];
    const migrationDown = [getRunSqlQuery(downSql, source)];
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
    dispatch(setSidebarLoading(true));
    dispatch({ type: SAVE_NEW_TABLE_NAME });
    const source = getState().tables.currentDataSource;

    const property = tableType.toLowerCase();
    if (oldName === newName) {
      dispatch(setSidebarLoading(false));
      return dispatch(
        showErrorNotification(
          `Renaming ${property} failed`,
          `The ${property} name is already ${oldName}`
        )
      );
    }

    if (!gqlPattern.test(newName)) {
      dispatch(setSidebarLoading(false));
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
    const migrateUp = [getRunSqlQuery(upSql, source)];
    const migrateDown = [getRunSqlQuery(downSql, source)];
    // apply migrations
    const migrationName = `rename_${compositeName}_${currentSchema}_${oldName}`;

    const requestMsg = `Renaming ${property}...`;
    const successMsg = `Renaming ${property} successful`;
    const errorMsg = `Renaming ${property} failed`;

    const customOnSuccess = () => {
      dispatch(_push(getSchemaBaseRoute(currentSchema, source))); // to avoid 404
      dispatch(updateSchemaInfo()).then(() => {
        dispatch(
          _push(getTableModifyRoute(currentSchema, source, newName, isTable))
        );
        dispatch(setSidebarLoading(false));
      });
    };
    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
      dispatch(setSidebarLoading(false));
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
    const source = getState().tables.currentDataSource;
    const triggerName = trigger.trigger_name;
    const triggerSchema = trigger.trigger_schema;

    const tableName = table.table_name;
    const tableSchema = table.table_schema;

    const upMigrationSql = dataSource.getDropTriggerSql(
      tableSchema,
      triggerName,
      tableName
    );
    const migrationUp = [getRunSqlQuery(upMigrationSql, source)];

    const downMigrationSql = dataSource.getCreateTriggerSql(
      tableName,
      tableSchema,
      triggerName,
      trigger
    );
    const migrationDown = [getRunSqlQuery(downMigrationSql, source)];

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
    dispatch(setSidebarLoading(true));
    const currentSchema = getState().tables.currentSchema;
    const source = getState().tables.currentDataSource;
    // handle no primary key
    const sqlDropTable = dataSource.getDropSql(tableName, currentSchema);

    const migration = new Migration();
    migration.add(getRunSqlQuery(sqlDropTable, source));

    const migrationName = `drop_table_${currentSchema}_${tableName}`;
    const requestMsg = 'Deleting table...';
    const successMsg = 'Table deleted';
    const errorMsg = 'Deleting table failed';

    const customOnSuccess = () => {
      dispatch(updateSchemaInfo());
      dispatch(_push(getSchemaBaseRoute(currentSchema, source)));
      dispatch(setSidebarLoading(false));
    };

    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
      dispatch(setSidebarLoading(false));
    };

    makeMigrationCall(
      dispatch,
      getState,
      migration.upMigration,
      migration.downMigration,
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
    dispatch(setSidebarLoading(true));
    const currentSchema = getState().tables.currentSchema;
    const currentDataSource = getState().tables.currentDataSource;
    const tableDef = generateTableDef(tableName, currentSchema);
    const { allSchemas } = getState().tables;
    const table = findTable(allSchemas, tableDef);
    const migration = new Migration();
    migration.add(
      getUntrackTableQuery(tableDef, currentDataSource),
      getTrackTableQuery({
        tableDef,
        source: currentDataSource,
        customColumnNames: escapeTableColumns(table),
        customName: escapeTableName(tableName),
      })
    );

    // apply migrations
    const migrationName = 'untrack_table_' + currentSchema + '_' + tableName;

    const requestMsg = 'Untracking table...';
    const successMsg = 'Table untracked';
    const errorMsg = 'Untrack table failed';

    const customOnSuccess = () => {
      dispatch(exportMetadata()).then(() => {
        dispatch(_push(getSchemaBaseRoute(currentSchema, currentDataSource)));
        dispatch(setSidebarLoading(false));
      });
    };
    const customOnError = err => {
      dispatch({ type: UPDATE_MIGRATION_STATUS_ERROR, data: err });
      dispatch(setSidebarLoading(false));
    };

    makeMigrationCall(
      dispatch,
      getState,
      migration.upMigration,
      migration.downMigration,
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
    const source = getState().tables.currentDataSource;
    const sqlQuery = dataSource.getViewDefinitionSql(viewName);
    const reqBody = getRunSqlQuery(sqlQuery, source, false, true);

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

        const viewType = data.result[1].length > 1 ? data.result[1][1] : 'VIEW';
        const fullName = '"' + currentSchema + '"."' + viewName + '"';
        let runSqlDef = '';

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
    dispatch(setSidebarLoading(true));
    const currentSchema = getState().tables.currentSchema;
    const source = getState().tables.currentDataSource;
    const property = viewType.toLowerCase();

    const sqlDropView = dataSource.getDropSql(
      viewName,
      currentSchema,
      viewType
    );
    const migration = new Migration();
    migration.add(getRunSqlQuery(sqlDropView, source)); // TODO Down migrations

    // Apply migrations
    const migrationName = 'drop_view_' + currentSchema + '_' + viewName;

    const requestMsg = `Deleting ${property}...`;
    const successMsg = `${capitalize(property)} deleted`;
    const errorMsg = `Deleting ${property} failed`;

    const customOnSuccess = () => {
      dispatch(_push(getSchemaBaseRoute(currentSchema, source)));
      dispatch(setSidebarLoading(false));
    };
    const customOnError = () => {
      dispatch(setSidebarLoading(false));
    };

    makeMigrationCall(
      dispatch,
      getState,
      migration.upMigration,
      migration.downMigration,
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
    const commonDataObject = { ...column, ...tableSchema };
    const source = getState().tables.currentDataSource;
    const name = commonDataObject.column_name;
    const tableName = commonDataObject.table_name;
    const currentSchema = commonDataObject.table_schema;
    const comment = commonDataObject.comment;
    const is_nullable = commonDataObject.is_nullable;
    const col_type = commonDataObject.data_type_name;
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

    const migration = new Migration();
    migration.add(
      getRunSqlQuery(
        dataSource.getDropColumnSql(tableName, currentSchema, name),
        source
      ),
      getRunSqlQuery(
        dataSource.getAddColumnSql(tableName, currentSchema, name, col_type),
        source
      )
    );

    if (is_nullable) {
      migration.UNSAFE_add(
        null,
        getRunSqlQuery(
          dataSource.getDropNotNullSql(tableName, currentSchema, name),
          source
        )
      );
    } else {
      migration.UNSAFE_add(null);
      getRunSqlQuery(
        dataSource.getSetNotNullSql(tableName, currentSchema, name),
        source
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
        const onUpdate = dataSource.getReferenceOption(fkc.on_update);
        const onDelete = dataSource.getReferenceOption(fkc.on_delete);

        migration.UNSAFE_add(
          null,
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
            ),
            source
          )
        );
      });
    }

    if (unique_constraints.length > 0) {
      unique_constraints.forEach(uc => {
        // add unique constraint to down migration
        migration.UNSAFE_add(
          null,
          getRunSqlQuery(
            dataSource.getAddUniqueConstraintSql(
              tableName,
              currentSchema,
              uc.constraint_name,
              uc.columns
            ),
            source
          )
        );
      });
    }

    if (commonDataObject.column_default !== null) {
      // add column default to down migration
      migration.UNSAFE_add(
        null,
        getRunSqlQuery(
          dataSource.getSetColumnDefaultSql(
            tableName,
            currentSchema,
            name,
            commonDataObject.column_default,
            col_type
          ),
          source
        )
      );
    }

    if (comment) {
      migration.UNSAFE_add(
        null,
        getRunSqlQuery(
          dataSource.getSetCommentSql(
            'column',
            tableName,
            currentSchema,
            comment,
            name,
            col_type
          ),
          source
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
      migration.upMigration,
      migration.downMigration,
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
    const source = getState().tables.currentDataSource;
    const constraint_name = `default_${source}_${currentSchema}_${tableName}_${colName}`;
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
      },
      currentDriver === 'mssql' ? constraint_name : undefined
    );

    const runSqlQueryDown = dataSource.getDropColumnSql(
      tableName,
      currentSchema,
      colName,
      { sqlGenerator: colDependentSQLGenerator }
    );

    const migration = new Migration();
    if (Array.isArray(sqlUp)) {
      const [extension, createCol] = sqlUp;
      migration.add(getRunSqlQuery(extension, source));
      migration.add(
        getRunSqlQuery(createCol, source),
        getRunSqlQuery(runSqlQueryDown, source)
      );
    } else {
      migration.add(getRunSqlQuery(sqlUp, source));
    }

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
      migration.upMigration,
      migration.downMigration,
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
    const source = getState().tables.currentDataSource;

    const migration = new Migration();
    migration.add(
      getRunSqlQuery(
        getRunSqlQuery(
          dataSource.getDropConstraintSql(tableName, currentSchema, cName),
          source
        )
      )
    );

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
      migration.upMigration,
      migration.downMigration,
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
    const source = getState().tables.currentDataSource;
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
    const migration = new Migration();
    migration.add(
      getRunSqlQuery(commentQueryUp, source),
      getRunSqlQuery(commentDownQuery, source)
    );

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
      migration.upMigration,
      migration.downMigration,
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
    const state = getState();
    const columnEdit = state.tables.modify.columnEdit[colName];
    const { currentDataSource, allSchemas } = state.tables;

    const onInvalidGqlColName = () =>
      dispatch(
        showErrorNotification(
          gqlColumnErrorNotif[3],
          gqlColumnErrorNotif[1],
          gqlColumnErrorNotif[2]
        )
      );
    const {
      migrationName,
      migration,
      metadataMigration,
    } = getColumnUpdateMigration(
      column,
      columnEdit,
      allSchemas,
      colName,
      onInvalidGqlColName,
      currentDataSource
    );
    // Apply migrations

    const requestMsg = 'Saving column changes...';
    const successMsg = 'Column modified';
    const errorMsg = 'Modifying column failed';

    const successCallback = () => {
      dispatch(resetColumnEdit(colName));
      onSuccess();
    };

    const makeMetadataMigrationCall = () => {
      makeMigrationCall(
        dispatch,
        getState,
        metadataMigration.migration.upMigration,
        metadataMigration.migration.downMigration,
        metadataMigration.migrationName,
        successCallback,
        () => {},
        'Saving column metadata changes...',
        'Column metadata modified',
        'Modifying column metadata failed'
      );
    };

    const customOnSuccess = () => {
      if (metadataMigration.migration.hasValue()) {
        dispatch(exportMetadata(makeMetadataMigrationCall));
      } else {
        successCallback();
      }
    };

    if (migration.hasValue()) {
      makeMigrationCall(
        dispatch,
        getState,
        migration.upMigration,
        migration.downMigration,
        migrationName,
        customOnSuccess,
        () => {},
        requestMsg,
        successMsg,
        errorMsg
      );
    } else if (metadataMigration.migration.hasValue()) {
      makeMetadataMigrationCall();
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
    const url = Endpoints.query;
    const source = getState().tables.currentDataSource;
    const reqQuery = getRunSqlQuery(dataSource.fetchColumnCastsQuery, source);
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
    const source = getState().tables.currentDataSource;
    const existingConstraint = existingConstraints[index];

    const migration = new Migration();
    migration.add(
      // Up migration: Drop the constraint
      getRunSqlQuery(
        dataSource.getDropConstraintSql(
          tableName,
          currentSchema,
          existingConstraint.constraint_name
        ),
        source
      ),
      // Down Migration: Create the constraint that is being dropped
      getRunSqlQuery(
        dataSource.getAddUniqueConstraintSql(
          tableName,
          currentSchema,
          getUniqueConstraintName(tableName, existingConstraint.columns),
          existingConstraint.columns.map(c => `"${c}"`)
        ),
        source
      )
    );

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
      migration.upMigration,
      migration.downMigration,
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

  const { currentTable, currentSchema, currentDataSource } = getState().tables;
  const { allSchemas } = getState().tables;
  const migration = new Migration();
  migration.add(
    getSetTableEnumQuery(
      generateTableDef(currentTable, currentSchema),
      !isEnum,
      currentDataSource
    ),
    getSetTableEnumQuery(
      generateTableDef(currentTable, currentSchema),
      isEnum,
      currentDataSource
    )
  );

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
    migration.upMigration,
    migration.downMigration,
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
  const source = getState().tables.currentDataSource;
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
        ),
        source
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
      ),
      source
    )
  );

  downQueries.push(
    getRunSqlQuery(
      dataSource.getDropConstraintSql(
        currentTable,
        currentSchema,
        newConstraint.name
      ),
      source
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
        ),
        source
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
    const source = getState().tables.currentDataSource;
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
        ),
        source
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
          ),
          source
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
          ),
          source
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
        ),
        source
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
  const source = getState().tables.currentDataSource;
  const viewDef = generateTableDef(viewName, schemaName);
  const view = findTable(getState().tables.allSchemas, viewDef);

  const existingCustomName = getTableCustomName(view);
  const existingCustomRootFields = getTableCustomRootFields(view);
  const existingColumnNames = getTableCustomColumnNames(view);

  const migration = new Migration();

  migration.add(
    getSetCustomRootFieldsQuery(
      viewDef,
      existingCustomRootFields,
      sanitiseColumnNames(customColumnNames),
      existingCustomName || null,
      source
    ),
    getSetCustomRootFieldsQuery(
      viewDef,
      existingCustomRootFields,
      existingColumnNames,
      existingCustomName,
      source
    )
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
    migration.upMigration,
    migration.downMigration,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

const saveIndex = (indexInfo, successCb, errorCb) => (dispatch, getState) => {
  if (!indexInfo) {
    return;
  }

  if (!dataSource.createIndexSql && !dataSource.dropIndexSql) {
    // ERROR: this datasource does not support creation/deletion of indexes
    return;
  }

  if (
    !indexInfo?.index_name?.trim() ||
    !indexInfo?.index_columns?.length ||
    !indexInfo?.index_type
  ) {
    dispatch(
      showErrorNotification(
        'Some required fields are missing',
        'Index Name, Index Columns and Index Type are all required fields'
      )
    );
    return;
  }

  const { currentSchema, currentTable, currentDataSource } = getState().tables;
  const upQueries = [];
  const downQueries = [];

  const upSql = dataSource.createIndexSql({
    table: { schema: currentSchema, name: currentTable },
    columns: indexInfo?.index_columns,
    indexName: indexInfo?.index_name?.trim(),
    indexType: indexInfo?.index_type,
    unique: indexInfo?.unique,
  });
  const downSql = dataSource.dropIndexSql(indexInfo?.index_name);

  upQueries.push(getRunSqlQuery(upSql, currentDataSource));
  downQueries.push(getRunSqlQuery(downSql, currentDataSource));

  const migrationName = `create_index_${indexInfo?.index_name || ''}`;
  const requestMsg = 'Creating index....';
  const successMsg = `Created index ${indexInfo?.index_name} successfully`;
  const errorMsg = 'Failed to create index';

  const customOnSuccess = () => successCb?.();

  const customOnError = () => errorCb?.();

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

const removeIndex = (indexInfo, successCb, errorCb) => (dispatch, getState) => {
  if (!indexInfo) {
    return;
  }

  if (!dataSource.createIndexSql && !dataSource.dropIndexSql) {
    // ERROR: this datasource does not support creation/deletion of indexes
    return;
  }

  const removeConfirmation = getConfirmation(
    `You want to remove the index: ${indexInfo?.index_name || ''}`
  );
  if (!removeConfirmation) {
    return;
  }

  const { currentTable, currentSchema, currentDataSource } = getState().tables;
  const upQueries = [];
  const downQueries = [];

  const upSql = dataSource.dropIndexSql(indexInfo?.index_name);
  const downSql = dataSource.createIndexSql({
    indexName: indexInfo?.index_name,
    indexType: indexInfo?.index_type,
    table: { schema: currentSchema, name: currentTable },
    columns: indexInfo?.index_columns,
    unique: indexInfo?.unique,
  });

  upQueries.push(getRunSqlQuery(upSql, currentDataSource));
  downQueries.push(getRunSqlQuery(downSql, currentDataSource));

  const migrationName = `drop_index_${indexInfo?.index_name || 'indexName'}`;
  const requestMsg = 'Removing index....';
  const successMsg = 'Removed index successfully';
  const errorMsg = 'Failed to remove index';

  const customOnSuccess = () => successCb?.();

  const customOnError = () => errorCb?.();

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
  SET_PRIMARY_KEYS,
  EDIT_COLUMN,
  RESET_COLUMN_EDIT,
  ADD_PRIMARY_KEY,
  REMOVE_PRIMARY_KEY,
  RESET_PRIMARY_KEY,
  DELETE_PK_WARNING,
  SET_FOREIGN_KEYS,
  RESET,
  SET_UNIQUE_KEYS,
  TOGGLE_ENUM,
  TOGGLE_ENUM_SUCCESS,
  TOGGLE_ENUM_FAILURE,
  MODIFY_ROOT_FIELD,
  SET_CHECK_CONSTRAINTS,
  MODIFY_TABLE_CUSTOM_NAME,
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
  modifyTableCustomName,
  saveIndex,
  removeIndex,
};
