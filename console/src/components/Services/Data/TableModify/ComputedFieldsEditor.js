import React from 'react';
import AceEditor from 'react-ace';

import styles from './ModifyTable.scss';
import { getConfirmation } from '../../../Common/utils/jsUtils';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RawSqlButton from '../Common/ReusableComponents/RawSqlButton';
import {
  findFunction,
  getFunctionDefinition,
  getSchemaName,
} from '../../../Common/utils/pgUtils';
import { deleteComputedField, saveComputedField } from './ModifyActions';

const ComputedFieldsEditor = ({
  table,
  currentSchema,
  functions,
  schemaList,
  dispatch,
}) => {
  const computedFields = table.computed_fields;

  const emptyComputedField = {
    computed_field_name: '',
    function_name: '',
    function_schema: currentSchema,
  };

  const [computedFieldsState, setComputedFieldsState] = React.useState(
    computedFields.concat({ ...emptyComputedField })
  );

  React.useEffect(() => {
    setComputedFieldsState(computedFields.concat({ ...emptyComputedField }));
  }, [computedFields]); // Only re-run the effect if computedFields change

  return computedFieldsState.map((computedField, i) => {
    const isLast = computedFields.length <= i;

    const origComputedField = isLast ? null : computedFields[i];
    let origComputedFieldName = '';
    let origComputedFieldFunctionName = '';
    if (origComputedField) {
      origComputedFieldName = origComputedField.computed_field_name;
      origComputedFieldFunctionName = origComputedField.function_name;
    }

    const computedFieldName = computedField.computed_field_name;
    const computedFieldFunctionName = computedField.function_name;
    const computedFieldFunctionSchema = computedField.function_schema;

    let computedFieldFunction = null;
    let computedFieldFunctionDefinition = null;
    if (functions) {
      computedFieldFunction = findFunction(
        functions,
        computedFieldFunctionName,
        computedFieldFunctionSchema
      );

      if (computedFieldFunction) {
        computedFieldFunctionDefinition = getFunctionDefinition(
          computedFieldFunction
        );
      }
    }

    let removeFunc;
    if (!isLast) {
      removeFunc = () => {
        const confirmMessage = `This will permanently delete the computed field "${computedFieldName}" from this table`;
        const isOk = getConfirmation(
          confirmMessage,
          true,
          origComputedFieldName
        );
        if (isOk) {
          dispatch(deleteComputedField(origComputedField, table));
        }
      };
    }

    let saveFunc;
    if (computedFieldName && computedFieldFunctionDefinition) {
      saveFunc = toggle => {
        dispatch(
          saveComputedField(computedField, table, origComputedField, toggle)
        );
      };
    }

    const collapsedLabel = () => {
      if (isLast) {
        if (!computedFields.length) {
          return <div>No computed fields</div>;
        }
        return null;
      }

      return (
        <div>
          <b>{origComputedFieldName}</b>&nbsp;-&nbsp;
          <i>{origComputedFieldFunctionName}</i>
        </div>
      );
    };

    // expand button text "View"
    // eslint-disable-next-line no-nested-ternary
    const expandButtonText = isLast
      ? computedFields.length
        ? 'Add a new computed field'
        : 'Add'
      : 'Edit';

    const expandedLabel = () => {
      if (isLast) {
        return null;
      }

      return <b>{origComputedFieldName}</b>;
    };

    const expandedContent = () => {
      const getFunctionDefinitionSection = () => {
        const modifyFunctionBtn = (
          <RawSqlButton
            dataTestId={`modify-function-${computedFieldFunctionName}`}
            customStyles={`${styles.display_inline} ${styles.add_mar_left}`}
            sql={computedFieldFunctionDefinition}
            dispatch={dispatch}
          >
            Modify
          </RawSqlButton>
        );

        return (
          <div>
            <div className={`${styles.add_mar_bottom_mid}`}>
              <b>Function definition: </b>
              {computedFieldFunctionDefinition && modifyFunctionBtn}
            </div>
            <AceEditor
              mode="sql"
              readOnly
              theme="github"
              name="computed_field_fn_def"
              value={computedFieldFunctionDefinition || '-- Function not found'}
              minLines={3}
              maxLines={100}
              width="100%"
              showPrintMargin={false}
              className={styles.add_mar_top_small}
            />
          </div>
        );
      };

      const handleNameChange = e => {
        const newState = [...computedFieldsState];

        newState[i] = {
          ...newState[i],
          computed_field_name: e.target.value,
        };

        setComputedFieldsState(newState);
      };

      const handleFnSchemaChange = e => {
        const newState = [...computedFieldsState];

        newState[i] = {
          ...newState[i],
          function_schema: e.target.value,
        };

        setComputedFieldsState(newState);
      };

      const handleFnNameChange = e => {
        const newState = [...computedFieldsState];

        newState[i] = {
          ...newState[i],
          function_name: e.target.value,
        };

        setComputedFieldsState(newState);
      };

      return (
        <div>
          <div className={`${styles.add_mar_bottom}`}>
            <div className={`${styles.add_mar_bottom_mid}`}>
              <b>Computed field name:</b>
            </div>
            <input
              type="text"
              value={computedFieldName}
              onChange={handleNameChange}
              className={`form-control ${styles.wd50percent}`}
            />
          </div>
          <div className={`${styles.add_mar_bottom}`}>
            <div className={`${styles.add_mar_bottom_mid}`}>
              <b>Function schema: </b>
            </div>
            <select
              value={computedFieldFunctionSchema || currentSchema}
              className={`${styles.select} form-control ${
                styles.add_pad_left
              } ${styles.wd50percent}`}
              data-test={'computed_field-fn-ref-schema'}
              onChange={handleFnSchemaChange}
            >
              {schemaList.map((s, j) => {
                const schemaName = getSchemaName(s);

                return (
                  <option key={j} value={schemaName}>
                    {schemaName}
                  </option>
                );
              })}
            </select>
          </div>
          <div className={`${styles.add_mar_bottom}`}>
            <div className={`${styles.add_mar_bottom_mid}`}>
              <b>Function name: </b>
            </div>
            <input
              type="text"
              value={computedFieldFunctionName}
              onChange={handleFnNameChange}
              className={`form-control ${styles.wd50percent}`}
            />
          </div>
          {getFunctionDefinitionSection()}
        </div>
      );
    };

    const resetComputedField = () => {
      const newState = [...computedFieldsState];

      newState[i] = {
        ...newState[i],
        ...(origComputedField || emptyComputedField),
      };

      setComputedFieldsState(newState);
    };

    return (
      <div key={`computed-field-${origComputedFieldName || i}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          expandedLabel={expandedLabel}
          collapsedLabel={collapsedLabel}
          collapseCallback={resetComputedField}
          property={`computed-field-${i}`}
          service="modify-table"
          expandButtonText={expandButtonText}
          saveFunc={saveFunc}
          removeFunc={removeFunc}
          isCollapsable
        />
      </div>
    );
  });
};

export default ComputedFieldsEditor;
