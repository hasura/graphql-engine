import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RootFieldEditor from '../Common/Components/RootFieldEditor';
import {
  modifyTableCustomName,
  modifyRootFields,
  setCustomRootFields,
} from './ModifyActions';

import { Dispatch } from '../../../../types';
import {
  CustomRootField,
  CustomRootFields,
} from '../../../../dataSources/types';
import {
  getTableCustomRootFieldComment,
  getTableCustomRootFieldName,
  setTableCustomRootFieldComment,
  setTableCustomRootFieldName,
} from '../../../../dataSources';

type RootFieldsEditorProps = {
  existingRootFields: CustomRootFields;
  rootFieldsEdit: CustomRootFields;
  dispatch: Dispatch;
  tableName: string;
  customName: string;
  existingCustomName?: string;
  tableSchema: string;
};

const RootFieldsEditor = ({
  existingRootFields,
  rootFieldsEdit,
  dispatch,
  tableName,
  customName,
  existingCustomName,
  tableSchema,
}: RootFieldsEditorProps) => {
  const setRootFieldsBulk = (rf: CustomRootFields) => {
    dispatch(modifyRootFields(rf));
  };

  const onRootFieldChange = (field: keyof CustomRootFields) => ({
    onNameChange: (e: React.ChangeEvent<HTMLInputElement>) => {
      const newRootFields = {
        ...rootFieldsEdit,
        [field]: setTableCustomRootFieldName(
          rootFieldsEdit[field],
          e.target.value
        ),
      };
      dispatch(modifyRootFields(newRootFields));
    },

    onCommentChange: (comment: string | null) => {
      const newRootFields = {
        ...rootFieldsEdit,
        [field]: setTableCustomRootFieldComment(rootFieldsEdit[field], comment),
      };
      dispatch(modifyRootFields(newRootFields));
    },
  });

  const onChangeCustomName = (newName: string) => {
    dispatch(modifyTableCustomName(newName));
  };

  const renderCustomRootFieldLabel = (
    rootFieldName: string,
    rootFieldConfig: string | CustomRootField
  ) => {
    const rfCustomName = getTableCustomRootFieldName(rootFieldConfig);
    const rfComment = getTableCustomRootFieldComment(rootFieldConfig);
    return (
      <div className="mb-xs" key={rootFieldName}>
        <span className="flex items-center">
          <span className="font-semibold mr-xs">{rootFieldName}</span>
          {rfCustomName ? (
            <>
              <span className="mr-xs">&rarr;</span>
              <span>{rfCustomName}</span>
            </>
          ) : null}
        </span>
        <span className="text-gray-600 text-sm">{rfComment}</span>
      </div>
    );
  };

  const collapsedLabel = () => {
    const customNameLabel = existingCustomName
      ? [renderCustomRootFieldLabel('custom_table_name', existingCustomName)]
      : [];

    const existingRootFieldLabels = Object.entries(existingRootFields).map(
      ([rootField, customRootField]) =>
        customRootField === null
          ? []
          : [renderCustomRootFieldLabel(rootField, customRootField)]
    );

    const allLabels = customNameLabel.concat(...existingRootFieldLabels);

    return <div>{allLabels}</div>;
  };

  const editorExpanded = () => (
    <RootFieldEditor
      disabled={false}
      tableName={tableName}
      tableSchema={tableSchema}
      rootFields={rootFieldsEdit}
      customName={customName}
      customNameOnChange={(e: React.ChangeEvent<HTMLInputElement>) => {
        onChangeCustomName(e.target.value);
      }}
      selectOnChange={onRootFieldChange('select')}
      selectByPkOnChange={onRootFieldChange('select_by_pk')}
      selectAggOnChange={onRootFieldChange('select_aggregate')}
      selectStreamOnChange={onRootFieldChange('select_stream')}
      insertOnChange={onRootFieldChange('insert')}
      insertOneOnChange={onRootFieldChange('insert_one')}
      updateOnChange={onRootFieldChange('update')}
      updateByPkOnChange={onRootFieldChange('update_by_pk')}
      deleteOnChange={onRootFieldChange('delete')}
      deleteByPkOnChange={onRootFieldChange('delete_by_pk')}
      updateManyOnChange={onRootFieldChange('update_many')}
    />
  );

  const expandCallback = () => {
    dispatch(modifyTableCustomName(existingCustomName));
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
