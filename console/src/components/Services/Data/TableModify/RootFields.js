import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RootFieldEditor from '../Common/ReusableComponents/RootFieldEditor';
import { modifyRootFields, setCustomRootFields } from './ModifyActions';
import { isEmpty } from '../../../Common/utils/jsUtils';

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

  const collapsedLabel = () => {
    const customRootFieldsLabel = [];

    Object.keys(existingRootFields).forEach(rootField => {
      const customRootField = existingRootFields[rootField];
      if (customRootField) {
        customRootFieldsLabel.push(
          <span key={rootField}>
            {!isEmpty(customRootFieldsLabel) && ', '}
            <i>{rootField}</i> &rarr; {customRootField}
          </span>
        );
      }
    });

    return isEmpty(customRootFieldsLabel)
      ? 'No root fields customised'
      : customRootFieldsLabel;
  };

  const editorExpanded = () => (
    <RootFieldEditor
      tableName={tableName}
      aliases={rootFieldsEdit}
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

  const expandCallback = () => {
    setRootFieldsBulk(existingRootFields);
  };

  const saveFunc = toggleEditor => {
    dispatch(setCustomRootFields(toggleEditor));
  };

  return (
    <ExpandableEditor
      editorExpanded={editorExpanded}
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
