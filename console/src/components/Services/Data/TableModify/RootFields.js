import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RootFieldEditor from '../Common/Components/RootFieldEditor';
import { modifyRootFields, setCustomRootFields } from './ModifyActions';
import { isEmpty } from '../../../Common/utils/jsUtils';

import styles from './ModifyTable.scss';

const RootFields = ({
  existingRootFields,
  rootFieldsEdit,
  dispatch,
  tableName,
}) => {
  const setRootFieldsBulk = rf => {
    dispatch(modifyRootFields(rf));
  };

  const onChange = (field, customField) => {
    const newRootFields = {
      ...rootFieldsEdit,
      [field]: customField,
    };
    dispatch(modifyRootFields(newRootFields));
  };

  const collapsedLabel = () => {
    const customRootFieldLabels = [];

    Object.keys(existingRootFields).forEach(rootField => {
      const customRootField = existingRootFields[rootField];
      if (customRootField) {
        customRootFieldLabels.push(
          <React.Fragment>
            {!isEmpty(customRootFieldLabels) && ', '}
            <span className={styles.display_inline} key={rootField}>
              <i>{rootField}</i> &rarr; {customRootField}
            </span>
          </React.Fragment>
        );
      }
    });

    if (isEmpty(customRootFieldLabels)) {
      customRootFieldLabels.push('No root fields customised');
    }

    return <span>{customRootFieldLabels}</span>;
  };

  const editorExpanded = () => (
    <RootFieldEditor
      tableName={tableName}
      rootFields={rootFieldsEdit}
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
      property="custom-root-fields"
      service="modify-table"
      isCollapsable
    />
  );
};

export default RootFields;
