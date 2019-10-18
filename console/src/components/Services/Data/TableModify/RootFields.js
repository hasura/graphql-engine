import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RootFieldEditor from '../Common/ReusableComponents/RootFieldEditor';
import { modifyRootFields, setCustomRootFields } from './ModifyActions';

const RootFields = ({ existingRootFields, rootFieldsEdit, dispatch }) => {
  const setRootFieldsBulk = rf => {
    dispatch(modifyRootFields(rf));
  };

  const onChange = (field, alias) => {
    const newRootFields = {
      ...rootFieldsEdit,
      [field]: alias,
    };
    dispatch(modifyRootFields(newRootFields));
  };

  const rootFields = (aliases, disabled) => {
    return (
      <RootFieldEditor
        aliases={aliases}
        disabled={disabled}
        selectOnChange={e => {
          onChange('select', e.target.value);
        }}
        selectByPkOnChange={e => {
          onChange('select_by_pk', e.target.value);
        }}
        selectAggOnChange={e => {
          onChange('select_aggregate', e.target.value);
        }}
        insertOnChange={e => {
          onChange('insert', e.target.value);
        }}
        updateOnChange={e => {
          onChange('update', e.target.value);
        }}
        deleteOnChange={e => {
          onChange('delete', e.target.value);
        }}
      />
    );
  };

  const editorExpanded = () => rootFields(rootFieldsEdit, false);
  const editorCollapsed = () => rootFields(existingRootFields, true);

  const expandCallback = () => {
    setRootFieldsBulk(existingRootFields);
  };

  const saveFunc = toggleEditor => {
    dispatch(setCustomRootFields(toggleEditor));
  };

  return (
    <ExpandableEditor
      editorExpanded={editorExpanded}
      editorCollapsed={editorCollapsed}
      expandCallback={expandCallback}
      saveFunc={saveFunc}
      property="root-field-alias"
      service="modify-table"
      isCollapsable
    />
  );
};

export default RootFields;
