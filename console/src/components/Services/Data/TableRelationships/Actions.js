import inflection from 'inflection';

import {
  makeMigrationCall,
  loadUntrackedRelations,
  RESET_MANUAL_REL_TABLE_LIST,
} from '../DataActions';
import gqlPattern, { gqlRelErrorNotif } from '../Common/GraphQLValidation';
import { showErrorNotification } from '../../Common/Notification';
import suggestedRelationshipsRaw from './autoRelations';

export const REL_SET_TYPE = 'ModifyTable/REL_SET_TYPE';
export const REL_SET_RTABLE = 'ModifyTable/REL_SET_RTABLE';
export const REL_SET_LCOL = 'ModifyTable/REL_SET_LCOL';
export const REL_SET_RCOL = 'ModifyTable/REL_SET_RCOL';
export const REL_RESET = 'ModifyTable/REL_RESET';
export const REL_SELECTION_CHANGED = 'ModifyTable/REL_SELECTION_CHANGED';
export const REL_NAME_CHANGED = 'ModifyTable/REL_NAME_CHANGED';
export const REL_ADD_NEW_CLICKED = 'ModifyTable/REL_ADD_NEW_CLICKED';
export const REL_SET_MANUAL_COLUMNS = 'ModifyTable/REL_SET_MANUAL_COLUMNS';
export const REL_ADD_MANUAL_CLICKED = 'ModifyTable/REL_ADD_MANUAL_CLICKED';

const resetRelationshipForm = () => ({ type: REL_RESET });
const addNewRelClicked = () => ({ type: REL_ADD_NEW_CLICKED });
const relManualAddClicked = () => ({ type: REL_ADD_MANUAL_CLICKED });
const relSelectionChanged = selectedRelationship => ({
  type: REL_SELECTION_CHANGED,
  rel: selectedRelationship,
});
const relNameChanged = relName => ({
  type: REL_NAME_CHANGED,
  relName,
});
const relTypeChange = isObjRel => ({
  type: REL_SET_TYPE,
  isObjRel: isObjRel === 'true',
});
const relRTableChange = rTable => ({ type: REL_SET_RTABLE, rTable });

const saveRenameRelationship = (oldName, newName, tableName, callback) => {
  return (dispatch, getState) => {
    const currentSchema = getState().tables.currentSchema;
    const migrateUp = [
      {
        type: 'rename_relationship',
        args: {
          table: {
            name: tableName,
            schema: currentSchema,
          },
          name: oldName,
          new_name: newName,
        },
      },
    ];
    const migrateDown = [
      {
        type: 'rename_relationship',
        args: {
          table: {
            name: tableName,
            schema: currentSchema,
          },
          name: newName,
          new_name: oldName,
        },
      },
    ];
    // Apply migrations
    const migrationName = `rename_relationship_${oldName}_to_${newName}_schema_${currentSchema}_table_${tableName}`;

    const requestMsg = 'Renaming relationship...';
    const successMsg = 'Relationship renamed';
    const errorMsg = 'Renaming relationship failed';

    const customOnSuccess = () => {
      callback();
    };
    const customOnError = () => {};

    // Rename relationship should fetch entire schema info.
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

const generateRelationshipsQuery = relMeta => {
  console.log(relMeta);
  if (relMeta.isObjRel) {
    const upQuery = {
      type: 'create_object_relationship',
      args: {
        name: relMeta.relName,
        table: {
          name: relMeta.lTable,
          schema: relMeta.lSchema,
        },
        using: {},
      },
    };
    const columnMaps = relMeta.lcol.map((column, index) => ({
      lcol: column,
      rcol: relMeta.rcol[index],
    }));
    if (columnMaps.length === 1) {
      upQuery.args.using = {
        foreign_key_constraint_on: relMeta.lcol[0],
      };
    } else {
      const columnReducer = (accumulator, val) => ({
        ...accumulator,
        [val.lcol]: val.rcol,
      });
      upQuery.args.using = {
        manual_configuration: {
          remote_table: {
            name: relMeta.rTable,
            schema: relMeta.rSchema,
          },
          column_mapping: columnMaps.reduce(columnReducer, {}),
        },
      };
    }
    const downQuery = {
      type: 'drop_relationship',
      args: {
        table: { name: relMeta.lTable, schema: relMeta.lSchema },
        relationship: relMeta.relName,
      },
    };
    return { upQuery, downQuery };
  }
  const upQuery = {
    type: 'create_array_relationship',
    args: {
      name: relMeta.relName,
      table: {
        name: relMeta.lTable,
        schema: relMeta.lSchema,
      },
      using: {},
    },
  };
  const columnMaps = relMeta.rcol.map((column, index) => ({
    rcol: column,
    lcol: relMeta.lcol[index],
  }));
  if (columnMaps.length === 1) {
    upQuery.args.using = {
      foreign_key_constraint_on: {
        table: {
          name: relMeta.rTable,
          schema: relMeta.rSchema,
        },
        column: relMeta.rcol[0],
      },
    };
  } else {
    const columnReducer = (accumulator, val) => ({
      ...accumulator,
      [val.lcol]: val.rcol,
    });
    upQuery.args.using = {
      manual_configuration: {
        remote_table: {
          name: relMeta.rTable,
          schema: relMeta.rSchema,
        },
        column_mapping: columnMaps.reduce(columnReducer, {}),
      },
    };
  }
  const downQuery = {
    type: 'drop_relationship',
    args: {
      table: { name: relMeta.lTable, schema: relMeta.lSchema },
      relationship: relMeta.relName,
    },
  };
  return { upQuery, downQuery };
};

const deleteRelMigrate = relMeta => (dispatch, getState) => {
  const { upQuery, downQuery } = generateRelationshipsQuery(relMeta);
  const relChangesUp = [downQuery];
  const relChangesDown = [upQuery];

  // Apply migrations
  const migrationName = `drop_relationship_${relMeta.relName}_${
    relMeta.lSchema
  }_table_${relMeta.lTable}`;

  const requestMsg = 'Deleting Relationship...';
  const successMsg = 'Relationship deleted';
  const errorMsg = 'Deleting relationship failed';

  const customOnSuccess = () => {
    dispatch(loadUntrackedRelations());
  };
  const customOnError = () => {};

  // Delete relationship should fetch entire schema info.
  makeMigrationCall(
    dispatch,
    getState,
    relChangesUp,
    relChangesDown,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    true
  );
};

const addRelNewFromStateMigrate = () => (dispatch, getState) => {
  const state = getState().tables.modify.relAdd;
  const { upQuery, downQuery } = generateRelationshipsQuery({
    lTable: state.lTable,
    lSchema: state.lSchema,
    isObjRel: state.isObjRel,
    relName: state.relName,
    lcol: state.lcol,
    rcol: state.rcol,
    rTable: state.rTable,
    rSchema: state.rSchema,
  });
  const relChangesUp = [upQuery];
  const relChangesDown = [downQuery];

  // Apply migrations
  const migrationName = `add_relationship_${state.name}_table_${
    state.lSchema
  }_${state.lTable}`;

  const requestMsg = 'Adding Relationship...';
  const successMsg = 'Relationship created';
  const errorMsg = 'Creating relationship failed';

  const customOnSuccess = () => {
    dispatch(
      loadUntrackedRelations({
        tables: [
          {
            table_schema: state.lSchema,
            table_name: state.lTable,
          },
        ],
      })
    ).then(() => {
      dispatch(resetRelationshipForm());
    });
  };
  const customOnError = () => {};

  // Rename relationship should fetch only current table schema info.
  makeMigrationCall(
    dispatch,
    getState,
    relChangesUp,
    relChangesDown,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    true
  );
};

const relTableChange = tableName => (dispatch, getState) => {
  // set table name state
  dispatch({ type: REL_SET_RTABLE, rTable: tableName });
  // fetch columns of the selected table
  const schemaName = getState().tables.modify.relAdd.manualRelInfo.remoteSchema;
  const tableSchema = getState().tables.allSchemas.find(
    t => t.table_name === tableName && t.table_schema === schemaName
  );
  if (tableSchema) {
    const tableColumns = tableSchema.columns;
    dispatch({ type: REL_SET_MANUAL_COLUMNS, data: tableColumns });
  } else {
    console.error(`cannot find table: ${tableName}`);
    dispatch({ type: REL_SET_MANUAL_COLUMNS, data: [] });
  }
};

const addRelViewMigrate = tableName => (dispatch, getState) => {
  const state = getState().tables.modify.relAdd;
  const currentSchema = getState().tables.currentSchema;
  const remoteSchema = getState().tables.modify.relAdd.manualRelInfo
    .remoteSchema;
  const isObjRel = state.isObjRel;
  const name = state.relName;
  const lcol = state.lcol;
  const rcol = state.rcol;
  const columnMapping = {};

  columnMapping[lcol] = rcol;
  const relChangesUp = [
    {
      type: isObjRel
        ? 'create_object_relationship'
        : 'create_array_relationship',
      args: {
        name,
        table: { name: tableName, schema: currentSchema },
        using: {
          manual_configuration: {
            remote_table: { name: state.rTable, schema: remoteSchema },
            column_mapping: columnMapping,
          },
        },
      },
    },
  ];
  const relChangesDown = [
    {
      type: 'drop_relationship',
      args: {
        table: { name: tableName, schema: currentSchema },
        relationship: name,
      },
    },
  ];

  // Apply migrations
  const migrationName = `create_relationship_${name}_${currentSchema}_table_${tableName}`;

  const requestMsg = 'Adding Relationship...';
  const successMsg = 'Relationship created';
  const errorMsg = 'Creating relationship failed';

  const customOnSuccess = () => {
    /* Adds reset manual relationship state and closes the rel block */
    dispatch({ type: RESET_MANUAL_REL_TABLE_LIST });
    dispatch(relManualAddClicked());
  };
  const customOnError = () => {};

  // perform validations and make call
  if (!name.trim()) {
    dispatch(
      showErrorNotification(
        'Error adding relationship!',
        'Please select a name for the relationship',
        '',
        { custom: 'Relationship name cannot be empty' }
      )
    );
  } else if (!gqlPattern.test(name)) {
    dispatch(
      showErrorNotification(
        gqlRelErrorNotif[0],
        gqlRelErrorNotif[1],
        gqlRelErrorNotif[2],
        gqlRelErrorNotif[3]
      )
    );
  } else {
    makeMigrationCall(
      dispatch,
      getState,
      relChangesUp,
      relChangesDown,
      migrationName,
      customOnSuccess,
      customOnError,
      requestMsg,
      successMsg,
      errorMsg
    );
  }
};

const sanitizeRelName = arg => arg.trim();

const fallBackRelName = (relMeta, existingFields, iterNumber = 0) => {
  let relName;
  const targetTable = sanitizeRelName(relMeta.rTable);
  if (relMeta.isObjRel) {
    const objLCol = sanitizeRelName(relMeta.lcol.join('_'));
    relName = `${inflection.singularize(targetTable)}_by_${objLCol}${
      iterNumber ? '_' + iterNumber : ''
    }`;
  } else {
    const arrRCol = sanitizeRelName(relMeta.rcol.join('_'));
    relName = `${inflection.pluralize(targetTable)}_by_${arrRCol}${
      iterNumber ? '_' + iterNumber : ''
    }`;
  }
  relName = inflection.camelize(relName, true);
  /*
   * Recurse until a unique relationship name is found and keep prefixing an integer at the end to fix collision
   * */
  return relName in existingFields
    ? fallBackRelName(relMeta, existingFields, ++iterNumber)
    : relName;
};

const formRelName = (relMeta, existingFields) => {
  try {
    let finalRelName;
    const targetTable = sanitizeRelName(relMeta.rTable);
    if (relMeta.isObjRel) {
      finalRelName = inflection.singularize(targetTable);
    } else {
      finalRelName = inflection.pluralize(targetTable);
    }

    /* Check if it is existing, fallback to guaranteed unique name */
    if (existingFields && finalRelName in existingFields) {
      finalRelName = fallBackRelName(relMeta, existingFields);
    }

    return finalRelName;
  } catch (e) {
    return '';
  }
};

const getExistingFieldsMap = tableSchema => {
  const fieldMap = {};

  tableSchema.relationships.forEach(tr => {
    fieldMap[tr.rel_name] = true;
  });

  tableSchema.columns.forEach(tc => {
    fieldMap[tc.column_name] = true;
  });

  return fieldMap;
};

const getAllUnTrackedRelations = (allSchemas, currentSchema) => {
  const trackedTables = allSchemas.filter(
    table => table.is_table_tracked && table.table_schema === currentSchema
  );
  const tableRelMapping = trackedTables.map(table => ({
    table_name: table.table_name,
    existingFields: getExistingFieldsMap(table),
    relations: suggestedRelationshipsRaw(
      table.table_name,
      allSchemas,
      currentSchema
    ),
  }));

  const bulkRelTrack = [];
  const bulkRelTrackDown = [];

  tableRelMapping.forEach(table => {
    // check relations.obj and relations.arr length and form queries
    if (table.relations.objectRel.length) {
      table.relations.objectRel.forEach(indivObjectRel => {
        indivObjectRel.relName = formRelName(
          indivObjectRel,
          table.existingFields
        );
        /* Added to ensure that fallback relationship name is created in case of tracking all relationship at once */
        table.existingFields[indivObjectRel.relName] = true;
        const { upQuery, downQuery } = generateRelationshipsQuery(
          indivObjectRel
        );

        const objTrack = {
          upQuery,
          downQuery,
          data: indivObjectRel,
        };

        bulkRelTrack.push(objTrack);
      });
    }

    if (table.relations.arrayRel.length) {
      table.relations.arrayRel.forEach(indivArrayRel => {
        indivArrayRel.relName = formRelName(
          indivArrayRel,
          table.existingFields
        );
        /* Added to ensure that fallback relationship name is created in case of tracking all relationship at once */
        table.existingFields[indivArrayRel.relName] = true;
        const { upQuery, downQuery } = generateRelationshipsQuery(
          indivArrayRel
        );

        const arrTrack = {
          upQuery,
          downQuery,
          data: indivArrayRel,
        };

        bulkRelTrack.push(arrTrack);
      });
    }
  });

  return { bulkRelTrack: bulkRelTrack, bulkRelTrackDown: bulkRelTrackDown };
};

const autoTrackRelations = autoTrackData => (dispatch, getState) => {
  const relChangesUp = autoTrackData.map(data => data.upQuery);
  const relChangesDown = autoTrackData.map(data => data.downQuery);
  // Apply migrations
  const migrationName = 'track_all_relationships';

  const requestMsg = 'Adding Relationship...';
  const successMsg = 'Relationship created';
  const errorMsg = 'Creating relationship failed';
  const customOnSuccess = () => {
    dispatch(loadUntrackedRelations());
  };
  const customOnError = () => {};

  makeMigrationCall(
    dispatch,
    getState,
    relChangesUp,
    relChangesDown,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg,
    true
  );
};

const autoAddRelName = obj => (dispatch, getState) => {
  const currentSchema = getState().tables.currentSchema;
  const relName = obj.upQuery.args.name;

  const relChangesUp = [obj.upQuery];
  const relChangesDown = [obj.downQuery];

  // Apply migrations
  const migrationName = `add_relationship_${relName}_table_${currentSchema}_${
    obj.data.tableName
  }`;

  const requestMsg = 'Adding Relationship...';
  const successMsg = 'Relationship created';
  const errorMsg = 'Creating relationship failed';

  const customOnSuccess = () => {
    Promise.all([dispatch(loadUntrackedRelations())]);
  };
  const customOnError = () => {};

  makeMigrationCall(
    dispatch,
    getState,
    relChangesUp,
    relChangesDown,
    migrationName,
    customOnSuccess,
    customOnError,
    requestMsg,
    successMsg,
    errorMsg
  );
};

export {
  deleteRelMigrate,
  addNewRelClicked,
  relTypeChange,
  relRTableChange,
  addRelViewMigrate,
  relTableChange,
  relSelectionChanged,
  addRelNewFromStateMigrate,
  relNameChanged,
  resetRelationshipForm,
  relManualAddClicked,
  autoTrackRelations,
  autoAddRelName,
  formRelName,
  getAllUnTrackedRelations,
  saveRenameRelationship,
  getExistingFieldsMap,
};
