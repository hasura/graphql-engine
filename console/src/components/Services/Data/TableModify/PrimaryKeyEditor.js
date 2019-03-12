import React from 'react';
import {
  DELETE_PK_WARNING,
  setPrimaryKeys,
  savePrimaryKeys,
} from './ModifyActions';
import PrimaryKeySelector from '../Common/ReusableComponents/PrimaryKeySelector';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import { showSuccessNotification } from '../Notification';

const PrimaryKeyEditor = ({
  tableSchema,
  pkModify,
  styles,
  dispatch,
  currentSchema,
}) => {
  const columns = tableSchema.columns;
  const tablePrimaryKeyColumns = tableSchema.primary_key
    ? tableSchema.primary_key.columns
    : [];
  const orderedCols = columns.map((c, _i) => ({
    name: c.column_name,
    type: c.data_type,
    index: _i,
  }));
  const orderedPks = tablePrimaryKeyColumns.map(pk => {
    return orderedCols.find(c => c.name === pk).index;
  });

  const pkConstraintName = tableSchema.primary_key
    ? tableSchema.primary_key.constraint_name
    : '';
  const pkConfigText = tablePrimaryKeyColumns.join(', ');
  const pkEditorCollapsedLabel = () => (
    <div>
      <div className="container-fluid">
        <div className="row">
          <h5 className={styles.padd_bottom}>
            {pkConfigText ? <i> ( {pkConfigText} ) </i> : 'No primary key'}
            &nbsp;
          </h5>
        </div>
      </div>
    </div>
  );
  const pkEditorExpandedLabel = () => (
    <h5 className={styles.padd_bottom}>
      <b> {pkConfigText && `( ${pkConfigText} )`}</b>
    </h5>
  );
  const pkEditorExpanded = () => (
    <div>
      <div className={`${styles.pkEditorExpanded}`}>
        <PrimaryKeySelector
          dispatch={dispatch}
          setPk={setPrimaryKeys}
          columns={orderedCols}
          primaryKeys={pkModify}
        />
      </div>
    </div>
  );

  const setPkEditState = () => {
    dispatch(setPrimaryKeys([...orderedPks.map(pk => pk.toString()), '']));
  };

  const resetPkEditState = () => {
    dispatch(setPrimaryKeys(['']));
  };

  const onSave = (e, confirmed) => {
    if (pkConstraintName && pkModify.length === 1 && !confirmed) {
      const isOk = window.confirm(DELETE_PK_WARNING);
      if (!isOk) {
        setPkEditState();
        return dispatch(showSuccessNotification('No changes'));
      }
    }
    dispatch(
      savePrimaryKeys(tableSchema.table_name, currentSchema, pkConstraintName)
    );
  };

  const onRemove = () => {
    let isOk;
    if (pkConstraintName) {
      isOk = window.confirm(DELETE_PK_WARNING);
      if (!isOk) {
        return dispatch(showSuccessNotification('No changes'));
      }
    }
    dispatch(setPrimaryKeys(['']));
    onSave(null, isOk);
  };

  return (
    <ExpandableEditor
      collapsedLabel={pkEditorCollapsedLabel}
      expandedLabel={pkEditorExpandedLabel}
      editorExpanded={pkEditorExpanded}
      property={'edit-pks'}
      ongoingRequest={'todo'}
      service="modify-table"
      saveFunc={onSave}
      removeFunc={onRemove}
      expandCallback={setPkEditState}
      collapseCallback={resetPkEditState}
    />
  );
};

export default PrimaryKeyEditor;
