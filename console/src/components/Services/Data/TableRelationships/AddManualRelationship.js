import React from 'react';

import { ordinalColSort } from '../utils';
import { addRelViewMigrate, resetManualRelationshipForm } from './Actions';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import ManualRelationshipSelector from './ManualRelationshipSelector';
import {
  getColumnName,
  getSchemaName,
  getSchemaTables,
  getTableColumnNames,
  getTableName,
  getTableSchema,
  getTrackedTables
} from '../../../Common/utils/pgUtils';
import { Text } from '../../../UIKit/atoms';
import styles from '../TableModify/ModifyTable.scss';

const AddManualRelationship = ({
  tableSchema,
  allSchemas,
  schemaList,
  relAdd,
  dispatch
}) => {
  const columns = tableSchema.columns.sort(ordinalColSort);

  // columns in the right order with their indices
  const orderedColumns = columns.map((c, i) => ({
    name: getColumnName(c),
    index: i
  }));

  relAdd.rSchema = relAdd.rSchema || getTableSchema(tableSchema);

  const refTables = {};
  const trackedSchemaTables = getTrackedTables(
    getSchemaTables(allSchemas, relAdd.rSchema)
  );
  trackedSchemaTables.forEach(
    ts => (refTables[getTableName(ts)] = getTableColumnNames(ts))
  );

  const orderedSchemaList = schemaList.map(s => getSchemaName(s)).sort();

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

  const expandedLabel = () => (
    <Text fontWeight="bold" display="inline-block">
      Configure relationship
    </Text>
  );

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
