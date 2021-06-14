import React, { useEffect } from 'react';
import {
  DELETE_PK_WARNING,
  setPrimaryKeys,
  savePrimaryKeys,
} from './ModifyActions';
import PrimaryKeySelector from '../Common/Components/PrimaryKeySelector';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import { showSuccessNotification } from '../../Common/Notification';
import { getUkeyPkeyConfig, getKeyDef } from '../Common/Components/utils';

import styles from './ModifyTable.scss';

import { getConfirmation } from '../../../Common/utils/jsUtils';

const PrimaryKeyEditor = ({
  tableSchema,
  pkModify,
  dispatch,
  currentSchema,
  readOnlyMode,
}) => {
  const columns = tableSchema.columns;
  const tablePrimaryKeyColumns = tableSchema.primary_key
    ? tableSchema.primary_key.columns
    : [];

  // generate columns in a proper order
  const orderedCols = columns.map((c, _i) => ({
    name: c.column_name,
    type: c.data_type,
    index: _i,
  }));

  // generate primary keys in the order respecting the column order
  const orderedPks = tablePrimaryKeyColumns.map(pk => {
    return orderedCols.find(c => c.name === pk).index;
  });

  const pkConstraintName = tableSchema.primary_key
    ? tableSchema.primary_key.constraint_name
    : '';

  // label next to the button when the editor is collapsed
  let pkConfigText;
  if (tableSchema.primary_key) {
    pkConfigText = getKeyDef(
      getUkeyPkeyConfig(tablePrimaryKeyColumns),
      pkConstraintName
    );
  }

  const pkEditorCollapsedLabel = () => (
    <div>{pkConfigText ? pkConfigText : 'No primary key'}</div>
  );

  // label next to the button when the editor is expanded
  const pkEditorExpandedLabel = () => (
    <div data-test="pk-config-text">{pkConfigText}</div>
  );

  // expanded editor content
  const pkEditorExpanded = () => (
    <div>
      <div className={`${styles.add_mar_top_small} ${styles.add_mar_bottom}`}>
        <PrimaryKeySelector
          dispatch={dispatch}
          setPk={setPrimaryKeys}
          columns={orderedCols}
          primaryKeys={pkModify}
        />
      </div>
    </div>
  );

  // set PK edit state when editor is expanded
  const setPkEditState = () => {
    dispatch(setPrimaryKeys([...orderedPks.map(pk => pk.toString()), '']));
  };

  // reset PK edit state when the editor is collapsed
  const resetPkEditState = () => {
    dispatch(setPrimaryKeys(['']));
  };

  // save
  const onSave = (e, confirmed) => {
    if (pkConstraintName && pkModify.length === 1 && !confirmed) {
      const isOk = getConfirmation(DELETE_PK_WARNING);
      if (!isOk) {
        setPkEditState();
        return dispatch(showSuccessNotification('No changes'));
      }
    }
    dispatch(
      savePrimaryKeys(tableSchema.table_name, currentSchema, pkConstraintName)
    );
  };

  useEffect(() => {
    setPkEditState();
  }, [columns.length]);

  // remove
  const onRemove = () => {
    if (pkConstraintName) {
      const isOk = getConfirmation(DELETE_PK_WARNING);
      if (isOk) {
        dispatch(setPrimaryKeys(['']));
        onSave(null, true);
      }
    }
  };

  // Toggle button text when the editor is expanded and collapsed
  const expandButtonText = pkConfigText ? 'Edit' : 'Add';
  const collapsedButtonText = pkConfigText ? 'Close' : 'Cancel';

  // Wrap inside an expandable editor
  return (
    <ExpandableEditor
      collapsedLabel={pkEditorCollapsedLabel}
      expandedLabel={pkEditorExpandedLabel}
      editorExpanded={pkEditorExpanded}
      readOnlyMode={readOnlyMode}
      property={'pks'}
      service="modify-table"
      saveFunc={onSave}
      removeFunc={onRemove}
      expandCallback={setPkEditState}
      collapseButtonText={collapsedButtonText}
      expandButtonText={expandButtonText}
      collapseCallback={resetPkEditState}
    />
  );
};

export default PrimaryKeyEditor;
