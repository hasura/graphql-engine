import { useState, useEffect } from 'react';
import Endpoints from '../../../../../Endpoints';
import requestAction from '../../../../../utils/requestAction';
import {
  showSuccessNotification,
  showErrorNotification,
} from '../../Notification';
import gqlPattern, { gqlRelErrorNotif } from '../../Common/GraphQLValidation';
import { LOAD_REMOTE_RELATIONSHIPS } from '../Actions';

const genLoadRemoteRelationshipsQuery = tableName => {
  return {
    type: 'select',
    args: {
      table: {
        schema: 'hdb_catalog',
        name: 'hdb_remote_relationship',
      },
      columns: ['table_name', 'rel_name', 'rel_def'],
      where: {
        table_name: tableName,
      },
    },
  };
};

const genDropRelationship = (tableName, schemaName, relName) => {
  return {
    type: 'drop_remote_relationship',
    args: {
      table: {
        name: tableName,
        schema: schemaName,
      },
      name: relName,
    },
  };
};

const loadRemoteSchemasQuery = {
  type: 'get_remote_schema_info',
  args: {},
};

const generateCreateRemoteRelationshipQuery = (
  name,
  tableName,
  fieldNamePath,
  inputField,
  columnName,
  schemaName,
  isNameSpaced
) => {
  const query = {
    type: 'create_remote_relationship',
    args: {
      name,
      table: {
        name: tableName,
        schema: schemaName,
      },
      using: {
        table: tableName,
        remote_field: isNameSpaced ? fieldNamePath[1] : fieldNamePath[0],
        input_field: inputField,
        column: columnName,
      },
    },
  };
  if (isNameSpaced) {
    query.args.using.namespace = fieldNamePath[0];
  }
  return query;
};

export const loadRemoteRelationships = tableName => {
  return dispatch => {
    return dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        body: JSON.stringify(genLoadRemoteRelationshipsQuery(tableName)),
      })
    ).then(
      data => {
        dispatch({
          type: LOAD_REMOTE_RELATIONSHIPS,
          data,
        });
      },
      error => {
        console.error(error);
      }
    );
  };
};

const loadRemoteSchemas = cb => {
  return dispatch => {
    return dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        body: JSON.stringify(loadRemoteSchemasQuery),
      })
    ).then(
      data => {
        cb({
          schemas: data,
        });
      },
      error => {
        console.error(error);
        cb({
          error: error,
        });
      }
    );
  };
};

export const useRemoteSchemas = dispatch => {
  const [remoteSchemas, setRemoteSchemas] = useState({});
  useEffect(() => {
    dispatch(loadRemoteSchemas(r => setRemoteSchemas(r)));
  }, []);
  return remoteSchemas;
};

export const useRemoteSchemasEdit = () => {
  const defaultState = {
    relName: '',
    schemaName: '',
    fieldNamePath: [],
    inputField: '',
    tableColumn: '',
    nested: false,
  };
  const [rsState, setRsState] = useState(defaultState);

  const {
    relName,
    schemaName,
    fieldNamePath,
    inputField,
    tableColumn,
    nested,
  } = rsState;
  const setRelName = e => {
    setRsState({
      ...rsState,
      relName: e.target.value,
    });
  };
  const setSchemaName = e => {
    setRsState({
      ...defaultState,
      relName: rsState.relName,
      schemaName: e.target.value,
    });
  };
  const setFieldNamePath = list => {
    setRsState({
      ...rsState,
      inputField: '',
      tableColumn: '',
      fieldNamePath: list,
    });
  };
  const setInputField = e => {
    setRsState({
      ...rsState,
      inputField: e.target.value,
    });
  };
  const setTableColumn = e => {
    setRsState({
      ...rsState,
      tableColumn: e.target.value,
    });
  };

  const setNested = value => {
    const fnp = rsState.fieldNamePath;
    setRsState({
      ...rsState,
      fieldNamePath: fnp[0] ? [fnp[0]] : [],
      nested: value,
    });
  };

  const reset = () => {
    setRsState({
      ...defaultState,
    });
  };

  return {
    relName,
    setRelName,
    schemaName,
    setSchemaName,
    fieldNamePath,
    setFieldNamePath,
    inputField,
    setInputField,
    tableColumn,
    setTableColumn,
    reset,
    nested,
    setNested,
  };
};

export const saveRemoteRelQuery = (
  name,
  tableSchema,
  fieldNamePath,
  inputField,
  columnName,
  successCb,
  errorCb
) => {
  return dispatch => {
    const tableName = tableSchema.table_name;
    const schemaName = tableSchema.schema_name;
    if (!name) {
      return dispatch(
        showErrorNotification('Relationship name cannot be empty')
      );
    }
    if (!gqlPattern.test(name)) {
      return dispatch(
        showErrorNotification(
          gqlRelErrorNotif[0],
          gqlRelErrorNotif[1],
          gqlRelErrorNotif[2],
          gqlRelErrorNotif[3]
        )
      );
    }
    if (fieldNamePath.length === 0) {
      return dispatch(showErrorNotification('Please select a field'));
    }
    if (!inputField) {
      return dispatch(showErrorNotification('Please select an input field'));
    }
    if (!columnName) {
      return dispatch(showErrorNotification('Please select a table column'));
    }
    dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        body: JSON.stringify(
          generateCreateRemoteRelationshipQuery(
            name,
            tableName,
            fieldNamePath,
            inputField,
            columnName,
            schemaName,
            fieldNamePath.length === 2
          )
        ),
      })
    ).then(
      () => {
        if (successCb) {
          successCb();
        }
        dispatch(loadRemoteRelationships(tableName));
        dispatch(showSuccessNotification('Remote relationship created'));
      },
      err => {
        console.error(err);
        if (errorCb) {
          errorCb();
        }
        dispatch(
          showErrorNotification(
            'Failed creating remote relationship',
            err.error
          )
        );
      }
    );
  };
};

export const getRemoteRelDef = relDef => {
  const { table, column, input_field, remote_field, namespace } = relDef;
  if (namespace) {
    return ` ${table} . ${column} → ${namespace} { ${remote_field} ( ${input_field} ) }`;
  }
  return ` ${table} . ${column} → ${remote_field} ( ${input_field} )`;
};

export const deleteRemoteRelationship = (tableSchema, name) => {
  return dispatch => {
    return dispatch(
      requestAction(Endpoints.query, {
        method: 'POST',
        body: JSON.stringify(
          genDropRelationship(
            tableSchema.table_name,
            tableSchema.schema_name,
            name
          )
        ),
      })
    ).then(
      () => {
        dispatch(loadRemoteRelationships(tableSchema.table_name));
        dispatch(showSuccessNotification('Remote relationship deleted'));
      },
      e => {
        console.error(e);
        dispatch(
          showErrorNotification('Failed deleting remote relationship', e.error)
        );
      }
    );
  };
};
