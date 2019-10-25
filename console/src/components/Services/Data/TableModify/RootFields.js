import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RootFieldEditor from '../Common/ReusableComponents/RootFieldEditor';
import { modifyRootFields, setCustomRootFields } from './ModifyActions';

const RootFields = ({
  existingRootFields,
  rootFieldsEdit,
  dispatch,
  tableName,
}) => {
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

  const note =
    'You can customize the GraphQL root fields associated with this table.';

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
        tableName={tableName}
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

  const collapsedLabel = () => <i>{note}</i>;

  return (
    <ExpandableEditor
      editorExpanded={editorExpanded}
      editorCollapsed={editorCollapsed}
      expandCallback={expandCallback}
      collapsedLabel={collapsedLabel}
      saveFunc={saveFunc}
      property="root-field-alias"
      service="modify-table"
      isCollapsable
    />
  );
};

export default RootFields;
