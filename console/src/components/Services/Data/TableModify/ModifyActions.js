import requestAction from '../../../../utils/requestAction';
import Endpoints, { globalCookiePolicy } from '../../../../Endpoints';
import {
  handleMigrationErrors,
  makeMigrationCall,
  LOAD_UNTRACKED_RELATIONS,
  fetchTableComment,
} from '../DataActions';
import _push from '../push';
import { SET_SQL } from '../RawSQL/Actions';
import {
  showErrorNotification,
  showSuccessNotification,
} from '../Notification';
import dataHeaders from '../Common/Headers';
import { UPDATE_MIGRATION_STATUS_ERROR } from '../../../Main/Actions';
import { getAllUnTrackedRelations } from '../TableRelationships/Actions';
import gqlPattern, {
  gqlTableErrorNotif,
  gqlViewErrorNotif,
  gqlColumnErrorNotif,
} from '../Common/GraphQLValidation';
import {
  pgConfTypes,
  generateFKConstraintName,
} from '../Common/ReusableComponents/utils';

const DELETE_PK_WARNING = `Are you sure? Deleting a primary key DISABLE ALL ROW EDIT VIA THE CONSOLE.
        Also, this will delete everything associated with the column (included related entities in other tables) permanently?`;

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
      ts => ts.table_name === tableName
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
          sql: `
          alter table "${schemaName}"."${tableName}" drop constraint "${constraintName}";
        `,
        },
      });
    }
    // skip creating a new config if no columns were selected
    if (numSelectedPkColumns) {
      migrationUp.push({
        type: 'run_sql',
        args: {
          sql: `
            alter table "${schemaName}"."${tableName}"
            add constraint "${tableName}_pkey" primary key ( ${selectedPkColumns.join(
  ', '
)} );
          `,
        },
      });
    }

    const migrationDown = [];
    // skip dropping in down migration if no constraint was created
    if (numSelectedPkColumns) {
      migrationDown.push({
        type: 'run_sql',
        args: {
          sql: `
          alter table "${schemaName}"."${tableName}" drop constraint "${tableName}_pkey";
        `,
        },
      });
    }
    // skip creating in down migration if no constraint was dropped in up migration
    if (constraintName) {
      migrationDown.push({
        type: 'run_sql',
        sql: `
          alter table "${schemaName}"."${tableName}"
          add constraint "${constraintName}" primary key ( ${tableSchema.primary_key.columns.join(
  ', '
)} );
        `,
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
    if (constraintName) {
      migrationUp.push({
        type: 'run_sql',
        args: {
          sql: `alter table "${schemaName}"."${tableName}" drop constraint ${constraintName};`,
        },
      });
    }

    const generatedConstraintName = generateFKConstraintName(
      tableName,
      lcols,
      tableSchema.foreign_key_constraints
    );

    migrationUp.push({
      type: 'run_sql',
      args: {
        sql: `alter table "${schemaName}"."${tableName}" add constraint ${constraintName ||
          generatedConstraintName} foreign key (${lcols.join(
          ', '
        )}) references "${refTableName}"(${rcols.join(
          ', '
        )}) on update ${onUpdate} on delete ${onDelete};`,
      },
    });
    const migrationDown = [
      {
        type: 'run_sql',
        args: {
          sql: `alter table "${schemaName}"."${tableName}" drop constraint ${constraintName ||
            generatedConstraintName};`,
        },
      },
    ];
    if (constraintName) {
      const oldConstraint = tableSchema.foreign_key_constraints[index];
      migrationDown.push({
        type: 'run_sql',
        args: {
          sql: `alter table "${schemaName}"."${tableName}" add constraint ${constraintName} foreign key (${Object.keys(
            oldConstraint.column_mapping
          )
            .map(lc => `"${lc}"`)
            .join(', ')}) references "${
            oldConstraint.ref_table
          }"(${Object.values(oldConstraint.column_mapping)
            .map(rc => `"${rc}"`)
            .join(', ')}) on update ${
            pgConfTypes[oldConstraint.on_update]
          } on delete ${pgConfTypes[oldConstraint.on_delete]};`,
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
          sql: `alter table "${schemaName}"."${tableName}" drop constraint ${
            oldConstraint.constraint_name
          };`,
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
            oldConstraint.ref_table
          }"(${Object.values(oldConstraint.column_mapping)
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

const changeTableOrViewName = (isTable, oldName, newName, callback) => {
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
          gqlValidationError[4],
          gqlValidationError[1],
          gqlValidationError[2],
          gqlValidationError[3]
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
      callback();
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
      errorMsg,
      true
    );
  };
};

// TABLE MODIFY
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
      errorMsg
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
      const allSchemas = getState().tables.allSchemas;
      const untrackedRelations = getAllUnTrackedRelations(
        allSchemas,
        currentSchema
      ).bulkRelTrack;
      dispatch({
        type: LOAD_UNTRACKED_RELATIONS,
        untrackedRelations: untrackedRelations,
      });
      dispatch(_push('/'));
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
      errorMsg
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
          showErrorNotification(
            'Fetching definition failed!',
            err.error,
            reqBody,
            err
          )
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

const deleteColumnSql = (tableName, colName) => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const deleteQueryUp =
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
      '"';
    const schemaChangesUp = [
      {
        type: 'run_sql',
        args: {
          sql: deleteQueryUp,
        },
      },
    ];

    /*
    const schemaChangesDown = [{
      type: 'run_sql',
      args: {
        'sql': deleteQueryDown
      }
    }];
    */

    // Apply migrations
    const migrationName =
      'alter_table_' +
      currentSchema +
      '_' +
      tableName +
      '_drop_column_' +
      colName;

    const requestMsg = 'Deleting Column...';
    const successMsg = 'Column deleted';
    const errorMsg = 'Deleting column failed';

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

const addColSql = (
  tableName,
  colName,
  colType,
  colNull,
  colUnique,
  colDefault,
  callback
) => {
  let defWithQuotes = "''";
  if (colType === 'text' && colDefault !== '') {
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
    const runSqlQueryDown =
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
      '"';
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
      dispatch(fetchTableComment(tableName)).then(() => {
        dispatch(activateCommentEdit(false, null));
      });
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

const isColumnUnique = (tableSchema, colName) => {
  return (
    tableSchema.unique_constraints.filter(constraint =>
      constraint.columns.includes(colName)
    ).length !== 0
  );
};

const saveColumnChangesSql = (colName, column, allowRename) => {
  // eslint-disable-line no-unused-vars
  return (dispatch, getState) => {
    const columnEdit = getState().tables.modify.columnEdit[colName];
    const { tableName } = columnEdit;
    const colType = columnEdit.type;
    const nullable = columnEdit.isNullable;
    const unique = columnEdit.isUnique;
    const def = columnEdit.default || '';
    const comment = columnEdit.comment || '';
    const newName = allowRename ? columnEdit.name : null;
    const currentSchema = columnEdit.schemaName;
    // ALTER TABLE <table> ALTER COLUMN <column> TYPE <column_type>;
    let defWithQuotes = "''";
    if (colType === 'text' && def !== '') {
      defWithQuotes = "'" + def + "'";
    } else {
      defWithQuotes = def;
    }
    // check if column type has changed before making it part of the migration
    const originalColType = column.data_type; // "value"
    const originalColDefault = column.column_default; // null or "value"
    const originalColComment = getState().tables.columnComments[colName]; // null or "value"
    const originalColNullable = column.is_nullable; // "YES" or "NO"
    const originalColUnique = isColumnUnique(
      getState().tables.allSchemas.find(
        table => table.table_name === tableName
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
            gqlColumnErrorNotif[4],
            gqlColumnErrorNotif[1],
            gqlColumnErrorNotif[2],
            gqlColumnErrorNotif[3]
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
      dispatch(setColumnEdit(columnEdit));
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

export {
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
};
