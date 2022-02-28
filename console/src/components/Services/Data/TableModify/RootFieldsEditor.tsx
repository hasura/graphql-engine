import React from 'react';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RootFieldEditor from '../Common/Components/RootFieldEditor';
import {
  modifyTableCustomName,
  modifyRootFields,
  setCustomRootFields,
} from './ModifyActions';

import { Dispatch } from '../../../../types';
import { CustomRootFields } from '../../../../dataSources/types';
import {
  getTableCustomRootFieldName,
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

  const onChange = (field: keyof CustomRootFields, customField: string) => {
    const newRootFields = {
      ...rootFieldsEdit,
      [field]: setTableCustomRootFieldName(rootFieldsEdit[field], customField),
    };
    dispatch(modifyRootFields(newRootFields));
  };

  const onChangeCustomName = (newName: string) => {
    dispatch(modifyTableCustomName(newName));
  };

  const getRootFieldNames = (
    rootFields: CustomRootFields
  ): Record<string, string> => {
    const rootFieldNames = Object.entries(rootFields).map(([key, value]) =>
      value ? { [key]: getTableCustomRootFieldName(value) } : {}
    );

    return Object.assign({}, ...rootFieldNames);
  };

  const collapsedLabel = () => {
    const customRootFieldLabels: React.ReactNode[] = [];

    if (existingCustomName) {
      customRootFieldLabels.push(
        <span className="flex items-center" key={existingCustomName}>
          <span className="font-semibold mr-xs">custom_table_name</span>{' '}
          <span className="mr-xs">&rarr;</span>{' '}
          <span>{existingCustomName}</span>
        </span>
      );
    }

    Object.entries(getRootFieldNames(existingRootFields)).forEach(
      ([rootField, customRootField]) => {
        customRootFieldLabels.push(
          <>
            <span className="flex items-center" key={rootField}>
              <span className="font-semibold mr-xs">{rootField}</span>
              <span className="mr-xs">&rarr;</span>
              <span>{customRootField}</span>
            </span>
          </>
        );
      }
    );

    return <div>{customRootFieldLabels}</div>;
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
