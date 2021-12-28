import React from 'react';
import { ordinalColSort } from '../utils';
import { addRelViewMigrate, resetManualRelationshipForm } from './Actions';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ManualRelationshipSelector from './ManualRelationshipSelector';
import {
  getSchemaTables,
  getTableColumnNames,
  getTrackedTables,
} from '../../../../dataSources';

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
  const trackedSchemaTables = getTrackedTables(
    getSchemaTables(allSchemas, relAdd.rSchema)
  );
  trackedSchemaTables.forEach(
    ts => (refTables[ts.table_name] = getTableColumnNames(ts))
  );

  const orderedSchemaList = schemaList.sort();

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
        Add a new relationship <b>manually</b>
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
