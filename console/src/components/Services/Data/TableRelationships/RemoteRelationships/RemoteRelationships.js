import React, { useEffect } from 'react';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import { buildClientSchema } from 'graphql';
import styles from '../../TableModify/ModifyTable.scss';
import RemoteRelationshipExplorer from './GraphQLSchemaExplorer';
import { parseRemoteRelationship, getRemoteRelConfig } from '../utils';
import {
  setRemoteRelationships,
  defaultRemoteRelationship,
  createRemoteRelationship,
  introspectRemoteSchema,
  dropRemoteRelationship,
} from '../Actions';

const RemoteRelationships = ({
  remoteRelationships,
  dispatch,
  tableSchema,
  remoteSchemas,
}) => {
  const existingRemoteRelationships = tableSchema.remote_relationships.map(
    rr => {
      return parseRemoteRelationship({
        remote_schema: rr.configuration.remote_schema,
        remote_field: rr.configuration.remote_field,
        name: rr.configuration.name,
      });
    }
  );
  existingRemoteRelationships.push({
    ...defaultRemoteRelationship,
  });
  useEffect(() => {
    dispatch(setRemoteRelationships(existingRemoteRelationships));
  }, []);

  const relationshipList = () => {
    const numRels = remoteRelationships.relationships.length;
    return remoteRelationships.relationships.map((remoteRelationship, i) => {
      return (
        <RemoteRelationshipEditor
          relationship={remoteRelationship}
          allRelationships={remoteRelationships.relationships}
          index={i}
          numRels={numRels}
          dispatch={dispatch}
          tableSchema={tableSchema}
          remoteSchemas={remoteSchemas}
          graphqlSchema={
            remoteRelationships.remoteSchema[remoteRelationship.remoteSchema]
          }
          loading={remoteRelationships.loading}
          key={existingRemoteRelationships[i].name || 'new-remote-rel'}
        />
      );
    });
  };

  return (
    <div>
      <div className={styles.add_mar_bottom}>
        Add relationship to a remote schema
      </div>
      <div>{relationshipList()}</div>
    </div>
  );
};

const RemoteRelationshipEditor = ({
  relationship,
  dispatch,
  allRelationships,
  index,
  numRels,
  tableSchema,
  remoteSchemas,
  graphqlSchema,
  loading,
}) => {
  const isLast = index === numRels - 1;

  const handleRelnameChange = e => {
    const newRelationships = JSON.parse(JSON.stringify(allRelationships));
    newRelationships[index].name = e.target.value;
    dispatch(setRemoteRelationships(newRelationships));
  };

  const handleRemoteSchemaChange = e => {
    const newRelationships = JSON.parse(JSON.stringify(allRelationships));
    if (e.target.value === newRelationships[index].remoteSchema) {
      return;
    }
    const relName = newRelationships[index].name;
    newRelationships[index] = JSON.parse(
      JSON.stringify(defaultRemoteRelationship)
    );
    newRelationships[index].name = relName;
    newRelationships[index].remoteSchema = e.target.value;
    dispatch(setRemoteRelationships(newRelationships));
    dispatch(introspectRemoteSchema(e.target.value));
  };

  const handleRemoteFieldChange = (fieldName, nesting, checked) => {
    const newRelationships = JSON.parse(JSON.stringify(allRelationships));
    const newRemoteField = newRelationships[index].remoteField.filter(
      rf => rf.name !== fieldName && rf.nesting < nesting
    );
    if (checked) {
      newRemoteField.push({
        name: fieldName,
        nesting,
        arguments: [],
      });
    }
    newRelationships[index].remoteField = newRemoteField;
    dispatch(setRemoteRelationships(newRelationships));
  };

  const handleArgChange = (
    fieldName,
    nesting,
    argName,
    argNesting,
    checked,
    parentArg
  ) => {
    const newRelationships = JSON.parse(JSON.stringify(allRelationships));
    const concernedRemoteField = newRelationships[index].remoteField.find(
      rf => rf.name === fieldName && nesting === rf.nesting
    );
    let concernedArgs = concernedRemoteField.arguments.filter(a => {
      return !(
        a.name === argName &&
        a.argNesting === argNesting &&
        a.parentArg === parentArg
      );
    });
    const removeChildrenArgs = p => {
      const childrenArgList = [];
      concernedArgs = concernedArgs.filter(ca => {
        if (ca.parentArg === p) {
          childrenArgList.push(ca.name);
          return false;
        }
        return true;
      });
      childrenArgList.forEach(ca => removeChildrenArgs(`${p}.${ca}`));
    };
    removeChildrenArgs(argName);
    if (checked) {
      concernedArgs.push({
        name: argName,
        argNesting,
        parentArg,
      });
    }
    concernedRemoteField.arguments = concernedArgs;
    newRelationships[index].remoteField = newRelationships[
      index
    ].remoteField.map(rf => {
      if (rf.name === fieldName && rf.nesting === nesting) {
        return concernedRemoteField;
      }
      return rf;
    });
    dispatch(setRemoteRelationships(newRelationships));
  };

  const handleColumnChange = (colName, fieldName, fieldNesting, arg) => {
    const newRelationships = JSON.parse(JSON.stringify(allRelationships));
    const concernedRemoteField = newRelationships[index].remoteField.find(
      rf => rf.name === fieldName && fieldNesting === rf.nesting
    );
    const concernedArgs = concernedRemoteField.arguments.filter(a => {
      return !(
        a.name === arg.name &&
        a.argNesting === arg.argNesting &&
        a.parentArg === arg.parentArg
      );
    });
    concernedArgs.push({
      name: arg.name,
      parentArg: arg.parentArg,
      argNesting: arg.argNesting,
      column: colName,
    });
    concernedRemoteField.arguments = concernedArgs;
    newRelationships[index].remoteField = newRelationships[
      index
    ].remoteField.map(rf => {
      if (rf.name === fieldName && rf.nesting === fieldNesting) {
        return concernedRemoteField;
      }
      return rf;
    });
    dispatch(setRemoteRelationships(newRelationships));
  };

  const relNameTextBox = () => {
    return (
      <div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b>Name</b>
          </div>
          <div>
            <input
              type="text"
              className={`form-control ${styles.wd300Px}`}
              placeholder="name"
              value={relationship.name}
              onChange={handleRelnameChange}
            />
          </div>
        </div>
      </div>
    );
  };

  const remoteSchemaSelect = () => {
    const placeHolder = !relationship.remoteSchema && (
      <option key="placeholder" value="">
        {' '}
        -- remote schema --
      </option>
    );
    const remoteSchemaOptions = remoteSchemas.map(s => {
      return (
        <option key={s} value={s}>
          {s}
        </option>
      );
    });
    return (
      <div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b>Remote Schema:</b>
          </div>
          <div>
            <select
              className={`form-control ${styles.wd300Px}`}
              value={relationship.remoteSchema}
              onChange={handleRemoteSchemaChange}
            >
              {placeHolder}
              {remoteSchemaOptions}
            </select>
          </div>
        </div>
      </div>
    );
  };

  const expandedContent = () => {
    return (
      <div>
        {relNameTextBox()}
        {remoteSchemaSelect()}
        <div>
          <div className={styles.add_mar_bottom_mid}>
            <b>Configuration</b>
          </div>
          <RemoteRelationshipExplorer
            schema={graphqlSchema ? buildClientSchema(graphqlSchema) : null}
            relationship={relationship}
            handleArgChange={handleArgChange}
            handleRemoteFieldChange={handleRemoteFieldChange}
            handleColumnChange={handleColumnChange}
            tableSchema={tableSchema}
            loading={loading}
          />
        </div>
      </div>
    );
  };

  const collapsedContent = () => null;

  const saveFunc = () => {
    const successCallback = () => {
      if (isLast) {
        const newRelationships = JSON.parse(JSON.stringify(allRelationships));
        newRelationships.push({
          ...defaultRemoteRelationship,
        });
        dispatch(setRemoteRelationships(newRelationships));
      }
    };
    dispatch(createRemoteRelationship(index, successCallback));
  };

  let removeFunc;
  if (!isLast) {
    removeFunc = () => {
      const isOk = window.confirm('Are you sure?');
      if (!isOk) return;
      const successCallback = () => {
        const newRelationships = JSON.parse(JSON.stringify(allRelationships));
        dispatch(
          setRemoteRelationships([
            ...newRelationships.slice(0, index),
            ...newRelationships.slice(index + 1),
          ])
        );
      };
      dispatch(dropRemoteRelationship(index, successCallback));
    };
  }

  const expandCallback = () => {
    if (isLast) return;
    if (!graphqlSchema) {
      dispatch(introspectRemoteSchema(relationship.remoteSchema));
    }
  };

  const expandButtonText = isLast
    ? numRels > 1
      ? 'Add a new remote relationship'
      : 'Add a remote relationship'
    : 'Edit';
  const collapseButtonText = isLast ? 'Cancel' : 'Close';

  console.log(expandButtonText);

  const collapsedLabel = () =>
    getRemoteRelConfig(relationship, tableSchema.table_name, styles);

  return (
    <ExpandableEditor
      editorExpanded={expandedContent}
      editorCollapsed={collapsedContent}
      property={'remote-relationship-add'}
      service="table-relationship"
      saveFunc={saveFunc}
      expandCallback={expandCallback}
      expandButtonText={expandButtonText}
      collapseButtonText={collapseButtonText}
      collapsedLabel={collapsedLabel}
      removeFunc={removeFunc}
    />
  );
};

export default RemoteRelationships;

/*
  1. In handleRemoteSchemaChange, whenever a remote schmea is changed, introspect the remote schema and store in state
  2. Onsave, create the remote relationship
  3. OnDelte, delete the remote relationship
  4. OnLoad, serialise the remote relationships into state
  5. OnEdit of remote schema, introspect
*/
