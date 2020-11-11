import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RootFieldEditor from '../Common/Components/RootFieldEditor';
import { modifyRootFields, setCustomRootFields } from './ModifyActions';
import { isEmpty } from '../../../Common/utils/jsUtils';

import styles from './ModifyTable.scss';
import { Dispatch } from '../../../../types';

type RootFieldsEditorProps = {
  existingRootFields: Record<string, string>;
  rootFieldsEdit: Record<string, string>;
  dispatch: Dispatch;
  tableName: string;
};

const RootFieldsEditor = ({
  existingRootFields,
  rootFieldsEdit,
  dispatch,
  tableName,
}: RootFieldsEditorProps) => {
  const setRootFieldsBulk = (rf: Record<string, string>) => {
    dispatch(modifyRootFields(rf));
  };

  const onChange = (field: string, customField: string) => {
    const newRootFields = {
      ...rootFieldsEdit,
      [field]: customField,
    };
    dispatch(modifyRootFields(newRootFields));
  };

  const collapsedLabel = () => {
    const customRootFieldLabels: React.ReactNode[] = [];

    Object.keys(existingRootFields).forEach(rootField => {
      const customRootField = existingRootFields[rootField];
      if (customRootField) {
        customRootFieldLabels.push(
          <>
            {!isEmpty(customRootFieldLabels) && ', '}
            <span className={styles.display_inline} key={rootField}>
              <i>{rootField}</i> &rarr; {customRootField}
            </span>
          </>
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
      disabled={false}
      tableName={tableName}
      rootFields={rootFieldsEdit}
      selectOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('select', e.target.value);
      }}
      selectByPkOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('select_by_pk', e.target.value);
      }}
      selectAggOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('select_aggregate', e.target.value);
      }}
      insertOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('insert', e.target.value);
      }}
      insertOneOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('insert_one', e.target.value);
      }}
      updateOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('update', e.target.value);
      }}
      updateByPkOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('update_by_pk', e.target.value);
      }}
      deleteOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('delete', e.target.value);
      }}
      deleteByPkOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChange('delete_by_pk', e.target.value);
      }}
    />
  );

  const expandCallback = () => {
    setRootFieldsBulk(existingRootFields);
  };

  const saveFunc = (toggleEditor: () => void) => {
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

export default RootFieldsEditor;
