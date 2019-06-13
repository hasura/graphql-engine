import React from 'react';
import { ordinalColSort } from '../utils';
import { addRelViewMigrate, resetManualRelationshipForm } from './Actions';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ManualRelationshipSelector from './ManualRelationshipSelector';

const AddManualRelationship = ({
  tableSchema,
  allSchemas,
  schemaList,
  relAdd,
  dispatch,
}) => {
  const styles = require('../TableModify/ModifyTable.scss');

  const columns = tableSchema.columns.sort(ordinalColSort);

  // columns in the right order with their indices
  const orderedColumns = columns.map((c, i) => ({
    name: c.column_name,
    index: i,
  }));

  relAdd.rSchema = relAdd.rSchema || tableSchema.table_schema;

  const refTables = {};
  allSchemas.forEach(ts => {
    if (ts.table_schema === relAdd.rSchema) {
      refTables[ts.table_name] = ts.columns.map(c => c.column_name);
    }
  });

  const orderedSchemaList = schemaList.map(s => s.schema_name).sort();

  const resetManualRel = () => {
    dispatch(resetManualRelationshipForm());
  };

  const saveFk = toggleEditor => {
    dispatch(addRelViewMigrate(tableSchema, toggleEditor));
  };

  const expandedContent = () => (
    <ManualRelationshipSelector
      tableSchema={tableSchema}
      relAdd={relAdd}
      schemaList={orderedSchemaList}
      refTables={refTables}
      orderedColumns={orderedColumns}
      dispatch={dispatch}
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
        collapseCallback={resetManualRel}
        saveFunc={saveFk}
        service="create"
        property="manual-rel"
      />
    </div>
  );
};

export default AddManualRelationship;
