import requestAction from '../../../../utils/requestAction';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import {
  updateSchemaInfo,
  handleMigrationErrors,
  makeMigrationCall,
  LOAD_SCHEMA,
} from '../DataActions';
import _push from '../push';
import { SET_SQL } from '../RawSQL/Actions';
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
} from '../Common/ReusableComponents/utils';

import { isPostgresFunction } from '../utils';

import {
  fetchColumnCastsQuery,
  convertArrayToJson,
  getCreatePkSql,
  getDropPkSql,
} from './utils';

const DELETE_PK_WARNING =
  'Without a Primary key there is no way to uniquely identify a row of a table. Are you sure?';

const VIEW_DEF_REQUEST_SUCCESS = 'ModifyTable/VIEW_DEF_REQUEST_SUCCESS';
const VIEW_DEF_REQUEST_ERROR = 'ModifyTable/VIEW_DEF_REQUEST_ERROR';

const SAVE_NEW_TABLE_NAME = 'ModifyTable/SAVE_NEW_TABLE_NAME';

const TABLE_COMMENT_EDIT = 'ModifyTable/TABLE_COMMENT_EDIT';
const TABLE_COMMENT_INPUT_EDIT = 'ModifyTable/TABLE_COMMENT_INPUT_EDIT';

const ADD_PRIMARY_KEY = 'ModifyTable/ADD_PRIMARY_KEY';
const REMOVE_PRIMARY_KEY = 'ModifyTable/REMOVE_PRIMARY_KEY';
const RESET_PRIMARY_KEY = 'ModifyTable/RESET_PRIMARY_KEY';
const SET_PRIMARY_KEYS = 'ModifyTable/SET_PRIMARY_KEYS';

const SET_COLUMN_EDIT = 'ModifyTable/SET_COLUMN_EDIT;';
const RESET_COLUMN_EDIT = 'ModifyTable/RESET_COLUMN_EDIT;';
const EDIT_COLUMN = 'ModifyTable/EDIT_COLUMN';

const SET_FOREIGN_KEYS = 'ModifyTable/SET_FOREIGN_KEYS';
const SAVE_FOREIGN_KEY = 'ModifyTable/SAVE_FOREIGN_KEY';
const REMOVE_FOREIGN_KEY = 'ModifyTable/REMOVE_FOREIGN_KEY';

const FETCH_COLUMN_TYPE_CASTS = 'ModifyTable/FETCH_COLUMN_TYPE_CASTS';
const FETCH_COLUMN_TYPE_CASTS_FAIL = 'ModifyTable/FETCH_COLUMN_TYPE_CASTS_FAIL';
const SET_UNIQUE_KEYS = 'ModifyTable/SET_UNIQUE_KEYS';
const SAVE_UNIQUE_KEY = 'ModifyTable/SAVE_UNIQUE_KEY';
const REMOVE_UNIQUE_KEY = 'ModifyTable/REMOVE_UNIQUE_KEY';

const RESET = 'ModifyTable/RESET';

const setForeignKeys = fks => ({
  type: SET_FOREIGN_KEYS,
  fks,
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
      migrationUp.push({
        type: 'run_sql',
        args: {
          sql: getDropPkSql({ schemaName, tableName, constraintName }),
        },
      });
    }
    // skip creating a new config if no columns were selected
    if (numSelectedPkColumns) {
      migrationUp.push({
        type: 'run_sql',
        args: {
          sql: getCreatePkSql({
            schemaName,
            tableName,
            selectedPkColumns,
            constraintName: `${tableName}_pkey`,
          }),
        },
      });
    }

    const migrationDown = [];
    // skip dropping in down migration if no constraint was created
    if (numSelectedPkColumns) {
      migrationDown.push({
        type: 'run_sql',
        args: {
          sql: getDropPkSql({
            schemaName,
            tableName,
            constraintName: `${tableName}_pkey`,
          }),
        },
      });
    }

    // skip creating in down migration if no constraint was dropped in up migration
    if (constraintName) {
      migrationDown.push({
        type: 'run_sql',
        sql: getCreatePkSql({
          schemaName,
          tableName,
          selectedPkColumns: tableSchema.primary_key.columns,
          constraintName,
        }),
      });
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
      // foreign key already exists, alter the foreign key
      const migrationUpAlterFKeySql = `
             alter table "${schemaName}"."${tableName}" drop constraint "${constraintName}",
             add constraint "${generatedConstraintName}" 
             foreign key (${lcols.join(', ')}) 
             references "${refSchemaName}"."${refTableName}"
             (${rcols.join(', ')}) on update ${onUpdate} on delete ${onDelete};
      `;

      migrationUp.push({
        type: 'run_sql',
        args: {
          sql: migrationUpAlterFKeySql,
        },
      });
    } else {
      // foreign key not found, create a new one
      const migrationUpCreateFKeySql = `
           alter table "${schemaName}"."${tableName}"
           add constraint "${generatedConstraintName}" 
           foreign key (${lcols.join(', ')}) 
           references "${refSchemaName}"."${refTableName}"
           (${rcols.join(', ')}) on update ${onUpdate} on delete ${onDelete};
      `;

      migrationUp.push({
        type: 'run_sql',
        args: {
          sql: migrationUpCreateFKeySql,
        },
      });
    }

    const migrationDown = [];

    if (constraintName) {
      // when foreign key is altered
      const oldConstraint = tableSchema.foreign_key_constraints[index];
      const migrationDownAlterFKeySql = `
          alter table "${schemaName}"."${tableName}" drop constraint "${generatedConstraintName}",
          add constraint "${constraintName}" 
          foreign key (${Object.keys(oldConstraint.column_mapping)
            .map(lc => `"${lc}"`)
            .join(', ')}) 
          references "${oldConstraint.ref_table_table_schema}"."${
        oldConstraint.ref_table
      }"
          (${Object.values(oldConstraint.column_mapping)
            .map(rc => `"${rc}"`)
            .join(', ')}) 
          on update ${pgConfTypes[oldConstraint.on_update]}
          on delete ${pgConfTypes[oldConstraint.on_delete]};
        `;

      migrationDown.push({
        type: 'run_sql',
        args: {
          sql: migrationDownAlterFKeySql,
        },
      });
    } else {
      // when foreign key is created
      const migrationDownDeleteFKeySql = `
          alter table "${schemaName}"."${tableName}" drop constraint "${generatedConstraintName}"
      `;

      migrationDown.push({
        type: 'run_sql',
        args: {
          sql: migrationDownDeleteFKeySql,
        },
      });
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
    const migrationUp = [
      {
        type: 'run_sql',
        args: {
          sql: `alter table "${schemaName}"."${tableName}" drop constraint "${
            oldConstraint.constraint_name
          }";`,
        },
      },
    ];
    const migrationDown = [
      {
        type: 'run_sql',
        args: {
          sql: `alter table "${schemaName}"."${tableName}" add foreign key (${Object.keys(
            oldConstraint.column_mapping
          )
            .map(lc => `"${lc}"`)
            .join(', ')}) references "${
            oldConstraint.ref_table_table_schema
          }"."${oldConstraint.ref_table}"(${Object.values(
            oldConstraint.column_mapping
          )
            .map(rc => `"${rc}"`)
            .join(', ')}) on update ${
            pgConfTypes[oldConstraint.on_update]
          } on delete ${pgConfTypes[oldConstraint.on_delete]};`,
        },
      },
    ];
    const migrationName = `delete_fk_${schemaName}_${tableName}_${
      oldConstraint.constraint_name
    }`;
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

const changeTableOrViewName = (isTable, oldName, newName) => {
  return (dispatch, getState) => {
    const property = isTable ? 'table' : 'view';

    dispatch({ type: SAVE_NEW_TABLE_NAME });

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
    const currentSchema = getState().tables.currentSchema;
    const migrateUp = [
      {
        type: 'run_sql',
        args: {
          sql: `alter ${property} "${currentSchema}"."${oldName}" rename to "${newName}";`,
        },
      },
    ];
    const migrateDown = [
      {
        type: 'run_sql',
        args: {
          sql: `alter ${property} "${currentSchema}"."${newName}" rename to "${oldName}";`,
        },
      },
    ];
    // apply migrations
    const migrationName = `rename_${property}_` + currentSchema + '_' + oldName;

    const requestMsg = `Renaming ${property}...`;
    const successMsg = `Renaming ${property} successful`;
    const errorMsg = `Renaming ${property} failed`;

    const customOnSuccess = () => {
      dispatch(_push('/schema/' + currentSchema)); // to avoid 404
      dispatch(updateSchemaInfo()).then(() => {
        dispatch(
          _push(
            '/schema/' +
              currentSchema +
              '/' +
              property +
              's/' +
              newName +
              '/modify'
          )
        );
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

    const upMigrationSql = `DROP TRIGGER "${triggerName}" ON "${tableSchema}"."${tableName}";`;

    const migrationUp = [
      {
        type: 'run_sql',
        args: {
          sql: upMigrationSql,
        },
      },
    ];

    let downMigrationSql = '';

    downMigrationSql += `CREATE TRIGGER "${triggerName}"
${trigger.action_timing} ${
      trigger.event_manipulation
    } ON "${tableSchema}"."${tableName}"
FOR EACH ${trigger.action_orientation} ${trigger.action_statement};`;

    if (trigger.comment) {
      downMigrationSql += `COMMENT ON TRIGGER "${triggerName}" ON "${tableSchema}"."${tableName}" 
IS '${trigger.comment}';`;
    }
    const migrationDown = [
      {
        type: 'run_sql',
        args: {
          sql: downMigrationSql,
        },
      },
    ];

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
    const sqlDropTable =
      'DROP TABLE ' + '"' + currentSchema + '"' + '.' + '"' + tableName + '"';
    const sqlUpQueries = [
      {
        type: 'run_sql',
        args: { sql: sqlDropTable },
      },
    ];
    // const sqlCreateTable = 'CREATE TABLE ' + '"' + tableName + '"' + '(' + tableColumns + ')';
    // const sqlDownQueries = [
    //   {
    //     type: 'run_sql',
    //     args: { 'sql': sqlCreateTable }
    //   }
    // ];

    // apply migrations
    const migrationName = 'drop_table_' + currentSchema + '_' + tableName;

    const requestMsg = 'Deleting table...';
    const successMsg = 'Table deleted';
    const errorMsg = 'Deleting table failed';

    const customOnSuccess = () => {
      dispatch(updateSchemaInfo());

      dispatch(_push('/'));
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
    const upQueries = [
      {
        type: 'untrack_table',
        args: {
          table: {
            name: tableName.trim(),
            schema: currentSchema,
          },
        },
      },
    ];
    const downQueries = [
      {
        type: 'add_existing_table_or_view',
        args: {
          name: tableName.trim(),
          schema: currentSchema,
        },
      },
    ];

    // apply migrations
    const migrationName = 'untrack_table_' + currentSchema + '_' + tableName;

    const requestMsg = 'Untracking table...';
    const successMsg = 'Table untracked';
    const errorMsg = 'Untrack table failed';

    const customOnSuccess = () => {
      // Combine foreign_key_constraints and opp_foreign_key_constraints to get merged table data
      const tableData = [];
      const allSchemas = getState().tables.allSchemas;
      const schemaInfo = allSchemas.find(
        schema =>
          schema.table_name === tableName &&
          schema.table_schema === currentSchema
      );
      schemaInfo.foreign_key_constraints.forEach(fk_obj => {
        tableData.push({
          table_name: fk_obj.ref_table,
          table_schema: fk_obj.ref_table_table_schema,
        });
      });
      schemaInfo.opp_foreign_key_constraints.forEach(fk_obj => {
        tableData.push({
          table_name: fk_obj.table_name,
          table_schema: fk_obj.table_schema,
        });
      });
      tableData.push({
        table_schema: currentSchema,
        table_name: tableName,
      });
      dispatch(
        updateSchemaInfo({
          tables: tableData,
        })
      ).then(() => {
        dispatch(_push('/'));
      });
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
    const sqlQuery =
      'select view_definition from information_schema.views where table_name = ' +
      "'" +
      viewName +
      "'";
    const reqBody = {
      type: 'run_sql',
      args: {
        sql: sqlQuery,
      },
    };

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
          dispatch(_push('/sql'));
        }

        const runSqlDef =
          'CREATE OR REPLACE VIEW ' +
          '"' +
          currentSchema +
          '"' +
          '.' +
          '"' +
          viewName +
          '"' +
          ' AS \n' +
          finalDef;
        dispatch({ type: SET_SQL, data: runSqlDef });
      },
      err => {
        dispatch(
          showErrorNotification('Fetching definition failed!', err.error, err)
        );
      }
    );
  };
};

const deleteViewSql = viewName => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const sqlDropView =
      'DROP VIEW ' + '"' + currentSchema + '"' + '.' + '"' + viewName + '"';
    const sqlUpQueries = [
      {
        type: 'run_sql',
        args: { sql: sqlDropView },
      },
    ];
    // const sqlCreateView = ''; //pending
    // const sqlDownQueries = [
    //   {
    //     type: 'run_sql',
    //     args: { 'sql': sqlCreateView }
    //   }
    // ];

    // Apply migrations
    const migrationName = 'drop_view_' + currentSchema + '_' + viewName;

    const requestMsg = 'Deleting view...';
    const successMsg = 'View deleted';
    const errorMsg = 'Deleting view failed';

    const customOnSuccess = () => {
      dispatch(_push('/'));
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
    const alterStatement =
      'ALTER TABLE ' + '"' + currentSchema + '"' + '.' + '"' + tableName + '" ';

    const schemaChangesUp = [
      {
        type: 'run_sql',
        args: {
          sql: alterStatement + 'DROP COLUMN ' + '"' + name + '" CASCADE',
        },
      },
    ];
    const schemaChangesDown = [];

    schemaChangesDown.push({
      type: 'run_sql',
      args: {
        sql: alterStatement + 'ADD COLUMN ' + '"' + name + '"' + ' ' + col_type,
      },
    });

    if (is_nullable) {
      schemaChangesDown.push({
        type: 'run_sql',
        args: {
          sql:
            alterStatement +
            'ALTER COLUMN ' +
            '"' +
            name +
            '" ' +
            'DROP NOT NULL',
        },
      });
    } else {
      schemaChangesDown.push({
        type: 'run_sql',
        args: {
          sql:
            alterStatement +
            'ALTER COLUMN ' +
            '"' +
            name +
            '" ' +
            'SET NOT NULL',
        },
      });
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
        schemaChangesDown.push({
          type: 'run_sql',
          args: {
            sql:
              alterStatement +
              'ADD CONSTRAINT ' +
              `${fkc.constraint_name} ` +
              'FOREIGN KEY ' +
              `(${lcol.join(', ')}) ` +
              'REFERENCES ' +
              `"${fkc.ref_table_table_schema}"."${fkc.ref_table}" ` +
              `(${rcol.join(', ')}) ` +
              `ON DELETE ${onDelete} ` +
              `ON UPDATE ${onUpdate}`,
          },
        });
      });
    }

    if (unique_constraints.length > 0) {
      unique_constraints.forEach(uc => {
        // add unique constraint to down migration
        schemaChangesDown.push({
          type: 'run_sql',
          args: {
            sql:
              alterStatement +
              'ADD CONSTRAINT ' +
              `${uc.constraint_name} ` +
              'UNIQUE ' +
              `(${uc.columns.join(', ')})`,
          },
        });
      });
    }

    if (column.column_default !== null) {
      // add column default to down migration
      schemaChangesDown.push({
        type: 'run_sql',
        args: {
          sql:
            alterStatement +
            'ALTER COLUMN ' +
            `"${name}" ` +
            'SET DEFAULT ' +
            column.column_default,
        },
      });
    }

    // COMMENT ON COLUMN my_table.my_column IS 'Employee ID number';
    if (comment) {
      schemaChangesDown.push({
        type: 'run_sql',
        args: {
          sql:
            'COMMENT ON COLUMN ' +
            '"' +
            currentSchema +
            '"' +
            '.' +
            '"' +
            tableName +
            '"' +
            '.' +
            '"' +
            name +
            '"' +
            ' ' +
            'IS ' +
            "'" +
            comment +
            "'",
        },
      });
    }

    // Apply migrations
    const migrationName =
      'alter_table_' + currentSchema + '_' + tableName + '_drop_column_' + name;

    const requestMsg = 'Deleting Column...';
    const successMsg = 'Column deleted';
    const errorMsg = 'Deleting column failed';

    const customOnSuccess = (data, consoleMode, migrationMode) => {
      if (consoleMode === 'cli' && migrationMode) {
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
  let defWithQuotes = "''";

  const checkIfFunctionFormat = isPostgresFunction(colDefault);
  if (colType === 'text' && colDefault !== '' && !checkIfFunctionFormat) {
    defWithQuotes = "'" + colDefault + "'";
  } else {
    defWithQuotes = colDefault;
  }

  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    let runSqlQueryUp =
      'ALTER TABLE ' +
      '"' +
      currentSchema +
      '"' +
      '.' +
      '"' +
      tableName +
      '"' +
      ' ADD COLUMN ' +
      '"' +
      colName +
      '"' +
      ' ' +
      colType;

    // check if nullable
    if (colNull) {
      // nullable
      runSqlQueryUp += ' NULL';
    } else {
      // not nullable
      runSqlQueryUp += ' NOT NULL';
    }

    // check if unique
    if (colUnique) {
      runSqlQueryUp += ' UNIQUE';
    }

    // check if default value
    if (colDefault !== '') {
      runSqlQueryUp += ' DEFAULT ' + defWithQuotes;
    }

    runSqlQueryUp += ';';

    const colDependentSQL = colDependentSQLGenerator
      ? colDependentSQLGenerator(currentSchema, tableName, colName)
      : null;

    if (colDependentSQL) {
      runSqlQueryUp += '\n';
      runSqlQueryUp += colDependentSQL.upSql;
    }

    const schemaChangesUp = [];

    if (colType === 'uuid' && colDefault !== '') {
      schemaChangesUp.push({
        type: 'run_sql',
        args: {
          sql: 'CREATE EXTENSION IF NOT EXISTS pgcrypto;',
        },
      });
    }

    schemaChangesUp.push({
      type: 'run_sql',
      args: {
        sql: runSqlQueryUp,
      },
    });

    let runSqlQueryDown = '';

    if (colDependentSQL) {
      runSqlQueryDown += colDependentSQL.downSql;
      runSqlQueryDown += '\n';
    }

    runSqlQueryDown +=
      'ALTER TABLE ' +
      '"' +
      currentSchema +
      '"' +
      '.' +
      '"' +
      tableName +
      '"' +
      ' DROP COLUMN ' +
      '"' +
      colName +
      '";';

    const schemaChangesDown = [
      {
        type: 'run_sql',
        args: {
          sql: runSqlQueryDown,
        },
      },
    ];

    // Apply migrations
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
    const dropContraintQuery =
      'ALTER TABLE ' +
      '"' +
      currentSchema +
      '"' +
      '.' +
      '"' +
      tableName +
      '"' +
      ' DROP CONSTRAINT ' +
      '"' +
      cName +
      '"';
    const schemaChangesUp = [
      {
        type: 'run_sql',
        args: {
          sql: dropContraintQuery,
        },
      },
    ];

    // pending
    const schemaChangesDown = [];

    // Apply migrations
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

const saveTableCommentSql = isTable => {
  return (dispatch, getState) => {
    let updatedComment = getState().tables.modify.tableCommentEdit.editedValue;
    if (!updatedComment) {
      updatedComment = '';
    }
    const currentSchema = getState().tables.currentSchema;
    const tableName = getState().tables.currentTable;

    const commentQueryBase =
      'COMMENT ON ' +
      (isTable ? 'TABLE' : 'VIEW') +
      ' ' +
      '"' +
      currentSchema +
      '"' +
      '.' +
      '"' +
      tableName +
      '"' +
      ' IS ';
    const commentUpQuery =
      updatedComment === ''
        ? commentQueryBase + 'NULL'
        : commentQueryBase + "'" + updatedComment + "'";

    const commentDownQuery = commentQueryBase + 'NULL';
    const schemaChangesUp = [
      {
        type: 'run_sql',
        args: {
          sql: commentUpQuery,
        },
      },
    ];
    const schemaChangesDown = [
      {
        type: 'run_sql',
        args: {
          sql: commentDownQuery,
        },
      },
    ];

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
    ).length !== 0
  );
};

const saveColumnChangesSql = (colName, column, onSuccess) => {
  // eslint-disable-line no-unused-vars
  return (dispatch, getState) => {
    const columnEdit = getState().tables.modify.columnEdit[colName];
    const { tableName } = columnEdit;
    const colType = columnEdit.type;
    const nullable = columnEdit.isNullable;
    const unique = columnEdit.isUnique;
    const def = columnEdit.default || '';
    const comment = columnEdit.comment || '';
    const newName = columnEdit.name;
    const currentSchema = columnEdit.schemaName;
    const checkIfFunctionFormat = isPostgresFunction(def);
    // ALTER TABLE <table> ALTER COLUMN <column> TYPE <column_type>;
    let defWithQuotes;
    if (colType === 'text' && !checkIfFunctionFormat) {
      defWithQuotes = `'${def}'`;
    } else {
      defWithQuotes = def;
    }
    // check if column type has changed before making it part of the migration
    const originalColType = column.data_type; // "value"
    const originalColDefault = column.column_default; // null or "value"
    const originalColComment = column.comment; // null or "value"
    const originalColNullable = column.is_nullable; // "YES" or "NO"
    const originalColUnique = isColumnUnique(
      getState().tables.allSchemas.find(
        table =>
          table.table_name === tableName && table.table_schema === currentSchema
      ),
      colName
    );

    /* column type up/down migration */
    const columnChangesUpQuery =
      'ALTER TABLE ' +
      '"' +
      currentSchema +
      '"' +
      '.' +
      '"' +
      tableName +
      '"' +
      ' ALTER COLUMN ' +
      '"' +
      colName +
      '"' +
      ' TYPE ' +
      colType +
      ';';
    const columnChangesDownQuery =
      'ALTER TABLE ' +
      '"' +
      currentSchema +
      '"' +
      '.' +
      '"' +
      tableName +
      '"' +
      ' ALTER COLUMN ' +
      '"' +
      colName +
      '"' +
      ' TYPE ' +
      column.data_type +
      ';';
    const schemaChangesUp =
      originalColType !== colType
        ? [
            {
              type: 'run_sql',
              args: {
                sql: columnChangesUpQuery,
              },
            },
          ]
        : [];
    const schemaChangesDown =
      originalColType !== colType
        ? [
            {
              type: 'run_sql',
              args: {
                sql: columnChangesDownQuery,
              },
            },
          ]
        : [];

    /* column default up/down migration */
    if (def.trim() !== '') {
      // ALTER TABLE ONLY <table> ALTER COLUMN <column> SET DEFAULT <default>;
      const columnDefaultUpQuery =
        'ALTER TABLE ONLY ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ALTER COLUMN ' +
        '"' +
        colName +
        '"' +
        ' SET DEFAULT ' +
        defWithQuotes +
        ';';
      let columnDefaultDownQuery =
        'ALTER TABLE ONLY ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ALTER COLUMN ' +
        '"' +
        colName +
        ' DROP DEFAULT;';

      // form migration queries
      if (
        column.column_default !== '' &&
        column.column_default === def.trim()
      ) {
        // default value unchanged
        columnDefaultDownQuery =
          'ALTER TABLE ONLY ' +
          '"' +
          currentSchema +
          '"' +
          '.' +
          '"' +
          tableName +
          '"' +
          ' ALTER COLUMN ' +
          '"' +
          colName +
          '"' +
          ' SET DEFAULT ' +
          defWithQuotes +
          ';';
      } else if (
        column.column_default !== '' &&
        column.column_default !== def.trim()
      ) {
        // default value has changed
        columnDefaultDownQuery =
          'ALTER TABLE ONLY ' +
          '"' +
          currentSchema +
          '"' +
          '.' +
          '"' +
          tableName +
          '"' +
          ' ALTER COLUMN ' +
          '"' +
          colName +
          '"' +
          ' SET DEFAULT ' +
          defWithQuotes +
          ';';
      } else {
        // there was no default value originally. so drop default.
        columnDefaultDownQuery =
          'ALTER TABLE ONLY ' +
          '"' +
          currentSchema +
          '"' +
          '.' +
          '"' +
          tableName +
          '"' +
          ' ALTER COLUMN ' +
          '"' +
          colName +
          '"' +
          ' DROP DEFAULT;';
      }

      // check if default is unchanged and then do a drop. if not skip
      if (originalColDefault !== def.trim()) {
        schemaChangesUp.push({
          type: 'run_sql',
          args: {
            sql: columnDefaultUpQuery,
          },
        });
        schemaChangesDown.push({
          type: 'run_sql',
          args: {
            sql: columnDefaultDownQuery,
          },
        });
      }
    } else {
      // ALTER TABLE <table> ALTER COLUMN <column> DROP DEFAULT;
      const columnDefaultUpQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ALTER COLUMN ' +
        '"' +
        colName +
        '"' +
        ' DROP DEFAULT;';
      if (column.column_default !== null) {
        const columnDefaultDownQuery =
          'ALTER TABLE ' +
          '"' +
          currentSchema +
          '"' +
          '.' +
          '"' +
          tableName +
          '"' +
          ' ALTER COLUMN ' +
          '"' +
          colName +
          '"' +
          ' SET DEFAULT ' +
          column.column_default +
          ';';
        schemaChangesDown.push({
          type: 'run_sql',
          args: {
            sql: columnDefaultDownQuery,
          },
        });
      }

      if (originalColDefault !== def.trim() && originalColDefault !== null) {
        schemaChangesUp.push({
          type: 'run_sql',
          args: {
            sql: columnDefaultUpQuery,
          },
        });
      }
    }

    /* column nullable up/down migration */
    if (nullable) {
      // ALTER TABLE <table> ALTER COLUMN <column> DROP NOT NULL;
      const nullableUpQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ALTER COLUMN ' +
        '"' +
        colName +
        '"' +
        ' DROP NOT NULL;';
      const nullableDownQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ALTER COLUMN ' +
        '"' +
        colName +
        '"' +
        ' SET NOT NULL;';
      // check with original null
      if (originalColNullable !== 'YES') {
        schemaChangesUp.push({
          type: 'run_sql',
          args: {
            sql: nullableUpQuery,
          },
        });
        schemaChangesDown.push({
          type: 'run_sql',
          args: {
            sql: nullableDownQuery,
          },
        });
      }
    } else {
      // ALTER TABLE <table> ALTER COLUMN <column> SET NOT NULL;
      const nullableUpQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ALTER COLUMN ' +
        '"' +
        colName +
        '"' +
        ' SET NOT NULL;';
      const nullableDownQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ALTER COLUMN ' +
        '"' +
        colName +
        '"' +
        ' DROP NOT NULL;';
      // check with original null
      if (originalColNullable !== 'NO') {
        schemaChangesUp.push({
          type: 'run_sql',
          args: {
            sql: nullableUpQuery,
          },
        });
        schemaChangesDown.push({
          type: 'run_sql',
          args: {
            sql: nullableDownQuery,
          },
        });
      }
    }

    /* column unique up/down migration */
    if (unique) {
      const uniqueUpQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ADD CONSTRAINT ' +
        '"' +
        tableName +
        '_' +
        colName +
        '_key"' +
        ' UNIQUE ' +
        '("' +
        colName +
        '")';
      const uniqueDownQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' DROP CONSTRAINT ' +
        '"' +
        tableName +
        '_' +
        colName +
        '_key"';
      // check with original unique
      if (!originalColUnique) {
        schemaChangesUp.push({
          type: 'run_sql',
          args: {
            sql: uniqueUpQuery,
          },
        });
        schemaChangesDown.push({
          type: 'run_sql',
          args: {
            sql: uniqueDownQuery,
          },
        });
      }
    } else {
      const uniqueDownQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' ADD CONSTRAINT ' +
        '"' +
        tableName +
        '_' +
        colName +
        '_key"' +
        ' UNIQUE ' +
        '("' +
        colName +
        '")';
      const uniqueUpQuery =
        'ALTER TABLE ' +
        '"' +
        currentSchema +
        '"' +
        '.' +
        '"' +
        tableName +
        '"' +
        ' DROP CONSTRAINT ' +
        '"' +
        tableName +
        '_' +
        colName +
        '_key"';
      // check with original unique
      if (originalColUnique) {
        schemaChangesUp.push({
          type: 'run_sql',
          args: {
            sql: uniqueUpQuery,
          },
        });
        schemaChangesDown.push({
          type: 'run_sql',
          args: {
            sql: uniqueDownQuery,
          },
        });
      }
    }

    /* column comment up/down migration */
    const columnCommentUpQuery =
      'COMMENT ON COLUMN ' +
      '"' +
      currentSchema +
      '"' +
      '.' +
      '"' +
      tableName +
      '"' +
      '.' +
      '"' +
      colName +
      '"' +
      ' IS ' +
      "'" +
      comment +
      "'";
    const columnCommentDownQuery =
      'COMMENT ON COLUMN ' +
      '"' +
      currentSchema +
      '"' +
      '.' +
      '"' +
      tableName +
      '"' +
      '.' +
      '"' +
      colName +
      '"' +
      ' IS ' +
      "'" +
      originalColComment +
      "'";

    // check if comment is unchanged and then do an update. if not skip
    if (originalColComment !== comment.trim()) {
      schemaChangesUp.push({
        type: 'run_sql',
        args: {
          sql: columnCommentUpQuery,
        },
      });
      schemaChangesDown.push({
        type: 'run_sql',
        args: {
          sql: columnCommentDownQuery,
        },
      });
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
      schemaChangesUp.push({
        type: 'run_sql',
        args: {
          sql: `alter table "${currentSchema}"."${tableName}" rename column "${colName}" to "${newName}";`,
        },
      });
      schemaChangesDown.push({
        type: 'run_sql',
        args: {
          sql: `alter table "${currentSchema}"."${tableName}" rename column "${newName}" to "${colName}";`,
        },
      });
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
    const reqQuery = {
      type: 'run_sql',
      args: {
        sql: fetchColumnCastsQuery,
      },
    };
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
    const existingConstraint = existingConstraints[index];

    // Up migration: Drop the constraint
    const sqlUp = [
      {
        type: 'run_sql',
        args: {
          sql: `alter table "${currentSchema}"."${tableName}" drop constraint "${
            existingConstraint.constraint_name
          }";`,
        },
      },
    ];

    // Down Migration: Create the constraint that is being dropped
    const sqlDown = [
      {
        type: 'run_sql',
        args: {
          sql: `alter table "${currentSchema}"."${tableName}" add constraint "${getUniqueConstraintName(
            tableName,
            existingConstraint.columns
          )}" unique (${existingConstraint.columns
            .map(c => `"${c}"`)
            .join(', ')});`,
        },
      },
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
      // success callback
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

    // Down migration
    const downMigration = [];
    // drop the newly created constraint
    downMigration.push({
      type: 'run_sql',
      args: {
        sql: `alter table "${currentSchema}"."${tableName}" drop constraint "${getUniqueConstraintName(
          tableName,
          columns
        )}";`,
      },
    });
    // if any constraint is being dropped, create it back
    if (index < numUniqueKeys - 1) {
      downMigration.push({
        type: 'run_sql',
        args: {
          sql: `alter table "${currentSchema}"."${tableName}" add constraint "${getUniqueConstraintName(
            tableName,
            existingConstraint.columns
          )}" unique (${existingConstraint.columns
            .map(c => `"${c}"`)
            .join(', ')});`,
        },
      });
    }

    // up migration
    const upMigration = [];
    // drop the old constraint if there is any
    if (index < numUniqueKeys - 1) {
      upMigration.push({
        type: 'run_sql',
        args: {
          sql: `alter table "${currentSchema}"."${tableName}" drop constraint "${
            existingConstraint.constraint_name
          }";`,
        },
      });
    }

    // create the new constraint
    upMigration.push({
      type: 'run_sql',
      args: {
        sql: `alter table "${currentSchema}"."${tableName}" add constraint "${getUniqueConstraintName(
          tableName,
          columns
        )}" unique (${columns.map(c => `"${c}"`).join(', ')});`,
      },
    });

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

export {
  FETCH_COLUMN_TYPE_CASTS,
  FETCH_COLUMN_TYPE_CASTS_FAIL,
  VIEW_DEF_REQUEST_SUCCESS,
  VIEW_DEF_REQUEST_ERROR,
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
  changeTableOrViewName,
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
};
