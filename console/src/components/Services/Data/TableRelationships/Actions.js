import {
  makeMigrationCall,
  loadUntrackedRelations,
  loadSchema,
  RESET_MANUAL_REL_TABLE_LIST,
} from '../DataActions';
import gqlPattern, { gqlRelErrorNotif } from '../Common/GraphQLValidation';
import { showErrorNotification } from '../Notification';
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

const generateRelationshipsQuery = (
  tableName,
  relName,
  lcol,
  rTable,
  rcol,
  isObjRel,
  currentSchema
) => {
  if (isObjRel) {
    const upQuery = {
      type: 'create_object_relationship',
      args: {
        name: relName,
        table: {
          name: tableName,
          schema: currentSchema,
        },
        using: {},
      },
    };
    const columnMaps = lcol.map((column, index) => ({
      lcol: column,
      rcol: rcol[index],
    }));
    if (columnMaps.length === 1) {
      upQuery.args.using = {
        foreign_key_constraint_on: lcol[0],
      };
    } else {
      const columnReducer = (accumulator, val) => ({
        ...accumulator,
        [val.lcol]: val.rcol,
      });
      upQuery.args.using = {
        manual_configuration: {
          remote_table: {
            name: rTable,
            schema: currentSchema,
          },
          column_mapping: columnMaps.reduce(columnReducer, {}),
        },
      };
    }
    const downQuery = {
      type: 'drop_relationship',
      args: {
        table: { name: tableName, schema: currentSchema },
        relationship: relName,
      },
    };
    return { upQuery, downQuery };
  }
  const upQuery = {
    type: 'create_array_relationship',
    args: {
      name: relName,
      table: {
        name: tableName,
        schema: currentSchema,
      },
      using: {},
    },
  };
  const columnMaps = rcol.map((column, index) => ({
    rcol: column,
    lcol: lcol[index],
  }));
  if (columnMaps.length === 1) {
    upQuery.args.using = {
      foreign_key_constraint_on: {
        table: {
          name: rTable,
          schema: currentSchema,
        },
        column: rcol[0],
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
          name: rTable,
          schema: currentSchema,
        },
        column_mapping: columnMaps.reduce(columnReducer, {}),
      },
    };
  }
  const downQuery = {
    type: 'drop_relationship',
    args: {
      table: { name: tableName, schema: currentSchema },
      relationship: relName,
    },
  };
  return { upQuery, downQuery };
};

const deleteRelMigrate = (tableName, relName, lcol, rtable, rcol, isObjRel) => (
  dispatch,
  getState
) => {
  const currentSchema = getState().tables.currentSchema;
  const { upQuery, downQuery } = generateRelationshipsQuery(
    tableName,
    relName,
    lcol,
    rtable,
    rcol,
    isObjRel,
    currentSchema
  );
  const relChangesUp = [downQuery];
  const relChangesDown = [upQuery];

  // Apply migrations
  const migrationName = `drop_relationship_${relName}_${currentSchema}_table_${tableName}`;

  const requestMsg = 'Deleting Relationship...';
  const successMsg = 'Relationship deleted';
  const errorMsg = 'Deleting relationship failed';

  const customOnSuccess = () => {};
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

const addRelNewFromStateMigrate = () => (dispatch, getState) => {
  const state = getState().tables.modify.relAdd;
  const currentSchema = getState().tables.currentSchema;
  const { upQuery, downQuery } = generateRelationshipsQuery(
    state.tableName,
    state.name,
    state.lcol,
    state.rTable,
    state.rcol,
    state.isObjRel,
    currentSchema
  );
  const relChangesUp = [upQuery];
  const relChangesDown = [downQuery];

  // Apply migrations
  const migrationName = `add_relationship_${
    state.name
  }_table_${currentSchema}_${state.tableName}`;

  const requestMsg = 'Adding Relationship...';
  const successMsg = 'Relationship created';
  const errorMsg = 'Creating relationship failed';

  const customOnSuccess = () => {
    dispatch(resetRelationshipForm());
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

const relTableChange = tableName => (dispatch, getState) => {
  // set table name state
  dispatch({ type: REL_SET_RTABLE, rTable: tableName });
  // fetch columns of the selected table
  const tableSchema = getState().tables.modify.relAdd.manualRelInfo.tables.find(
    t => t.table_name === tableName
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
  const name = state.name;
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

const sanitizeRelName = arg =>
  arg
    .trim()
    .toLowerCase()
    .replace(/([^A-Z]+)(.)/gi, function modifyRel() {
      return arguments[2].toUpperCase();
    });

const formRelName = relMeta => {
  let finalRelName;
  // remove special chars and change first letter after underscore to uppercase
  const targetTable = sanitizeRelName(relMeta.rTable);
  if (relMeta.isObjRel) {
    const objLCol = sanitizeRelName(relMeta.lcol.join(','));
    finalRelName = `${targetTable}By${objLCol}`;
  } else {
    const arrRCol = sanitizeRelName(relMeta.rcol.join(','));
    finalRelName =
      `${
        targetTable
        // (targetTable[targetTable.length - 1] !== 's' ? 's' : '') + // add s only if the last char is not s
      }s` + `By${arrRCol}`;
  }
  return finalRelName;
};

const getAllUnTrackedRelations = (allSchemas, currentSchema) => {
  const tableRelMapping = allSchemas.map(table => ({
    table_name: table.table_name,
    relations: suggestedRelationshipsRaw(table.table_name, allSchemas),
  }));
  const bulkRelTrack = [];
  const bulkRelTrackDown = [];
  tableRelMapping.map(table => {
    // check relations.obj and relations.arr length and form queries
    if (table.relations.objectRel.length) {
      table.relations.objectRel.map(indivObjectRel => {
        const { upQuery, downQuery } = generateRelationshipsQuery(
          indivObjectRel.tableName,
          formRelName(indivObjectRel),
          indivObjectRel.lcol,
          indivObjectRel.rTable,
          indivObjectRel.rcol,
          true,
          currentSchema
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
      table.relations.arrayRel.map(indivArrayRel => {
        const { upQuery, downQuery } = generateRelationshipsQuery(
          indivArrayRel.tableName,
          formRelName(indivArrayRel),
          indivArrayRel.lcol,
          indivArrayRel.rTable,
          indivArrayRel.rcol,
          false,
          currentSchema
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
    errorMsg
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
    Promise.all([dispatch(loadSchema()), dispatch(loadUntrackedRelations())]);
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
};
