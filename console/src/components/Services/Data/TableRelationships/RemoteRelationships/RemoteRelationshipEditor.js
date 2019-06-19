import React from 'react';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import RemoteRelationshipExplorer from './GraphQLSchemaExplorer';
import styles from '../../TableModify/ModifyTable.scss';
import {
  relName as relNameTooltip,
  remoteSchema as remoteSchemaTooltip,
  configuration as configTooltip,
} from './Tooltips';
import { getRemoteRelConfig } from '../utils';
import {
  setRemoteRelationships,
  defaultRemoteRelationship,
  createRemoteRelationship,
  dropRemoteRelationship,
} from '../Actions';

const RemoteRelationshipEditor = ({
  relationship,
  dispatch,
  allRelationships,
  index,
  numRels,
  tableSchema,
  remoteSchemas,
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
          <div
            className={`${styles.add_mar_bottom_mid} ${styles.display_flex}`}
          >
            <div className={styles.add_mar_right_small}>
              <b>Name</b>
            </div>
            <div>{relNameTooltip(tableSchema.table_name)}</div>
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
          <div
            className={`${styles.add_mar_bottom_mid} ${styles.display_flex} ${
              styles.add_mar_right_small
            }`}
          >
            <div className={styles.add_mar_right_small}>
              <b>Remote Schema:</b>
            </div>
            <div>{remoteSchemaTooltip(tableSchema.table_name)}</div>
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
          <div
            className={`${styles.add_mar_bottom_mid} ${styles.display_flex} ${
              styles.add_mar_right_small
            }`}
          >
            <div className={styles.add_mar_right_small}>
              <b>Configuration:</b>
            </div>
            <div>{configTooltip()}</div>
          </div>
          <RemoteRelationshipExplorer
            dispatch={dispatch}
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

  const saveFunc = toggle => {
    const successCallback = () => {
      if (isLast) {
        const newRelationships = JSON.parse(JSON.stringify(allRelationships));
        newRelationships.push({
          ...defaultRemoteRelationship,
        });
        dispatch(setRemoteRelationships(newRelationships));
      }
      toggle();
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

  const expandButtonText = isLast
    ? numRels > 1
      ? 'Add a new remote relationship'
      : 'Add a remote relationship'
    : 'Edit';
  const collapseButtonText = isLast ? 'Cancel' : 'Close';

  const collapsedLabel = () =>
    getRemoteRelConfig(relationship, tableSchema.table_name, styles);

  return (
    <ExpandableEditor
      editorExpanded={expandedContent}
      property={'remote-relationship-add'}
      service="table-relationship"
      saveFunc={saveFunc}
      expandButtonText={expandButtonText}
      collapseButtonText={collapseButtonText}
      collapsedLabel={collapsedLabel}
      removeFunc={removeFunc}
    />
  );
};

export default RemoteRelationshipEditor;
