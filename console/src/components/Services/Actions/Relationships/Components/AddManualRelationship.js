import React from 'react';
import { addActionRel } from '../../ServerIO';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import ManualRelationshipSelector from './ManualRelationshipSelector';

const AddManualRelationship = ({
  objectType,
  allTables,
  schemaList,
  dispatch,
  stateCb,
}) => {
  const styles = require('../../Actions.scss');

  const saveFunc = toggleEditor => {
    dispatch(addActionRel(objectType, toggleEditor));
  };

  const expandedContent = () => (
    <ManualRelationshipSelector
      objectType={objectType}
      schemaList={schemaList}
      allTables={allTables}
      dispatch={dispatch}
      stateCb={stateCb}
    />
  );

  const expandedLabel = () => {
    return <b>Configure relationship</b>;
  };

  return (
    <div key="add_manual_relationship">
      <div className={styles.add_mar_bottom}>
        <label> Add a new relationship manually </label>
      </div>
      <ExpandableEditor
        editorExpanded={expandedContent}
        expandedLabel={expandedLabel}
        expandButtonText="Configure"
        saveFunc={saveFunc}
        service="create"
        property="manual-rel"
      />
    </div>
  );
};

export default AddManualRelationship;
