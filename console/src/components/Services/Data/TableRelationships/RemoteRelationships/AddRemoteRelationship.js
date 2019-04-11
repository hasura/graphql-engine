import React from 'react';
import ExpandableEditor from '../../../../Common/Layout/ExpandableEditor/Editor';
import styles from '../../TableModify/ModifyTable.scss';
import { showErrorNotification } from '../../Notification';
import {
  useRemoteSchemasEdit,
  useRemoteSchemas,
  saveRemoteRelQuery,
} from './remoteRelationshipUtils';

const AddRemoteRelationship = ({ dispatch, tableSchema }) => {
  const schemaInfo = useRemoteSchemas(dispatch).schemas || [];
  const {
    relName,
    setRelName,
    schemaName,
    setSchemaName,
    fieldNamePath,
    setFieldNamePath,
    inputField,
    setInputField,
    tableColumn,
    setTableColumn,
    reset,
    nested,
    setNested,
  } = useRemoteSchemasEdit();
  const remoteSchemas = schemaInfo
    .filter(s => s.schema_name !== 'hasura')
    .map(s => s.schema_name);
  let schema = {};

  let fields = [];
  if (schemaName) {
    schema = schemaInfo.find(s => s.schema_name === schemaName);
    fields = schema.fields.map(f => f.name);
  }

  let selectedField;
  let hasChildren;
  let inputFields = [];
  let childrenFields = [];
  const fieldPathLength = fieldNamePath.length;
  if (fieldPathLength > 0) {
    selectedField = fieldNamePath[0];
    const parentField = schema.fields.find(f => f.name === fieldNamePath[0]);
    if (parentField.selection_fields.length > 0) {
      hasChildren = true;
      childrenFields = parentField.selection_fields.map(f => f.name);
    }
    if (fieldPathLength === 1) {
      inputFields = Object.keys(parentField.input_types);
    } else {
      const selectedChildField = parentField.selection_fields.find(
        sf => sf.name === fieldNamePath[1]
      );
      inputFields = Object.keys(selectedChildField.input_types);
    }
  }

  const setFieldNameInFieldPath = (name, i = 0) => {
    if (i === 0) {
      setFieldNamePath([name]);
    } else {
      setFieldNamePath([...fieldNamePath.slice(0, i), name]);
    }
  };

  const columns = tableSchema.columns.map(c => c.column_name);
  const expanded = () => {
    const getNestedOptions = () => {
      let addNestingButton = null;
      let nestingDropdown = null;
      let removeNestingButton = null;
      if (hasChildren) {
        addNestingButton = (
          <div
            className={`col-md-1 ${styles.display_flex_centered} ${
              styles.add_mar_top_small
            } ${styles.padd_right_remove}`}
            onClick={() => {
              setNested(true);
            }}
          >
            <i className={`fa fa-plus ${styles.cursorPointer}`} />
          </div>
        );
      }
      if (nested && hasChildren) {
        addNestingButton = (
          <div
            className={`col-md-1 ${styles.display_flex_centered} ${
              styles.add_mar_top_small
            } ${styles.padd_right_remove}`}
          >
            <b>.</b>
          </div>
        );
        nestingDropdown = (
          <div className={`col-md-3 ${styles.padd_right_remove}`}>
            <select
              className={`form-control ${styles.wd150px}`}
              value={fieldNamePath[1] || ''}
              onChange={e => setFieldNameInFieldPath(e.target.value, 1)}
            >
              <option key="empty_key" value="" disabled>
                -- nested field --
              </option>
              {childrenFields.sort().map(t => {
                return (
                  <option key={t} value={t}>
                    {t}
                  </option>
                );
              })}
            </select>
          </div>
        );
        removeNestingButton = (
          <div
            className={`col-md-1 ${styles.cursorPointer} ${
              styles.display_flex_centered
            } ${styles.add_mar_top_small}`}
            onClick={() => {
              setNested(false);
            }}
          >
            <i className="fa fa-times" />
          </div>
        );
      }
      return [addNestingButton, nestingDropdown, removeNestingButton];
    };

    return (
      <div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b> Relationship name </b>
          </div>
          <div>
            <input
              className={`form-control ${styles.wd150px}`}
              type="text"
              placeholder="name"
              value={relName}
              onChange={setRelName}
            />
          </div>
        </div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b> Remote schema </b>
          </div>
          <div>
            <select
              className={`form-control ${styles.wd150px}`}
              value={schemaName || ''}
              onChange={setSchemaName}
            >
              <option key="empty_key" value="" disabled>
                -- remote schema --
              </option>
              {remoteSchemas.sort().map(rs => {
                return (
                  <option key={rs} value={rs}>
                    {rs}
                  </option>
                );
              })}
            </select>
          </div>
        </div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b>Field name</b>
          </div>
          <div className={'row'}>
            <div className={'col-md-3'}>
              <select
                className={`form-control ${styles.wd150px}`}
                value={selectedField || ''}
                onChange={e => setFieldNameInFieldPath(e.target.value)}
                title={!schemaName ? 'Select remote schema first' : undefined}
                disabled={!schemaName}
              >
                <option key="empty_key" value="" disabled>
                  -- field name --
                </option>
                {fields.sort().map(t => {
                  return (
                    <option key={t} value={t}>
                      {t}
                    </option>
                  );
                })}
              </select>
            </div>
            {getNestedOptions()}
          </div>
        </div>
        <div className={`${styles.add_mar_bottom}`}>
          <div className={`${styles.add_mar_bottom_mid}`}>
            <b>Mapping</b>
            <i> (from table column to input field)</i>
          </div>
          <div className={'row'}>
            <div className={'col-md-3'}>
              <select
                className={`form-control ${styles.wd150px}`}
                value={tableColumn || ''}
                onChange={setTableColumn}
                title={
                  fieldNamePath.length === 0
                    ? 'Select remote schema and field name first'
                    : undefined
                }
                disabled={fieldNamePath.length === 0}
              >
                <option key="select_type" value="">
                  -- table column --
                </option>
                {columns.sort().map(c => {
                  return (
                    <option key={c} value={c}>
                      {c}
                    </option>
                  );
                })}
              </select>
            </div>
            <div
              className={`col-md-1 ${styles.display_flex_centered} ${
                styles.add_mar_top_small
              } ${styles.padd_right_remove}`}
            >
              <i className="fa fa-arrow-right" />
            </div>
            <div className={'col-md-3'}>
              <select
                className={`form-control ${styles.wd150px}`}
                value={inputField || ''}
                onChange={setInputField}
                title={
                  fieldNamePath.length === 0
                    ? 'Select remote schema and field name first'
                    : undefined
                }
                disabled={fieldNamePath.length === 0}
              >
                <option key="select_type" value="" disabled>
                  -- input field --
                </option>
                {inputFields.sort().map(inpF => {
                  return (
                    <option key={inpF} value={inpF}>
                      {inpF}
                    </option>
                  );
                })}
              </select>
            </div>
          </div>
        </div>
      </div>
    );
  };
  const expandButtonText = '+ Add a remote relationship';
  const saveFunc = toggle => {
    if (!schemaName) {
      return dispatch(showErrorNotification('Please select a remote schema'));
    }
    dispatch(
      saveRemoteRelQuery(
        relName,
        tableSchema,
        fieldNamePath,
        inputField,
        tableColumn,
        () => {
          reset();
          setNested(false);
          toggle();
        }
      )
    );
  };

  return (
    <div>
      <ExpandableEditor
        expandButtonText={expandButtonText}
        editorExpanded={expanded}
        service="relationships"
        property="remote-add"
        collapseButtonText="Cancel"
        saveFunc={saveFunc}
        collapseCallback={reset}
      />
    </div>
  );
};

export default AddRemoteRelationship;
