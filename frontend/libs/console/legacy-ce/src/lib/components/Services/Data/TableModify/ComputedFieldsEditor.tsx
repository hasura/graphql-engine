import React from 'react';
import AceEditor from 'react-ace';
import { LearnMoreLink } from '../../../../new-components/LearnMoreLink';

import { getConfirmation } from '../../../Common/utils/jsUtils';
import ExpandableEditor from '../../../Common/Layout/ExpandableEditor/Editor';
import RawSqlButton from '../Common/Components/RawSqlButton';
import Tooltip from '../../../Common/Tooltip/Tooltip';
import { dataSource, getComputedFieldComment } from '../../../../dataSources';
import { deleteComputedField, saveComputedField } from './ModifyActions';
import { fetchFunctionInit } from '../DataActions';
import SearchableSelectBox from '../../../Common/SearchableSelect/SearchableSelect';
import { Dispatch } from '../../../../types';
import { Schema, ComputedField, Table } from '../../../../dataSources/types';
import { PGFunction } from '../../../../dataSources/services/postgresql/types';
import { CommentInput } from '../Common/Components/CommentInput';
import { inputStyles } from '../constants';

interface ComputedFieldsEditorProps {
  table: Table;
  currentSchema: string;
  functions: PGFunction[];
  schemaList: Schema[];
  dispatch: Dispatch;
  source: string;
}

const ComputedFieldsEditor: React.FC<ComputedFieldsEditorProps> = ({
  table,
  currentSchema,
  functions,
  schemaList,
  dispatch,
  source,
}) => {
  const computedFields = table.computed_fields;
  const defaultSchemaForCurrentDataSource = dataSource.defaultRedirectSchema;

  const emptyComputedField: ComputedField = {
    computed_field_name: '',
    definition: {
      function: {
        name: '',
        schema: currentSchema,
      },
      table_argument: null,
      session_argument: null,
    },
    comment: null,
  };

  // State management - start

  const getStateComputedFields = () => {
    return computedFields.concat({ ...emptyComputedField });
  };

  const [stateComputedFields, setComputedFieldsState] = React.useState(
    getStateComputedFields()
  );

  React.useEffect(() => {
    setComputedFieldsState(getStateComputedFields());
  }, [computedFields]);

  // State management - end

  const fieldsEditor = stateComputedFields.map((computedField, i) => {
    const isLast = computedFields.length <= i;

    const origComputedField = isLast ? null : computedFields[i];
    let origComputedFieldName = '';
    let origComputedFieldFunctionName = '';
    let origComputedFieldComment: string | null = '';
    if (origComputedField) {
      const origComputedFieldFunctionDef =
        origComputedField.definition.function;

      origComputedFieldName = origComputedField.computed_field_name;
      origComputedFieldFunctionName = origComputedFieldFunctionDef.name;
      origComputedFieldComment = getComputedFieldComment(origComputedField);
    }

    const computedFieldFunctionDef = computedField.definition.function;

    const computedFieldName = computedField.computed_field_name;
    const computedFieldFunctionName = computedFieldFunctionDef.name;
    const computedFieldFunctionSchema = computedFieldFunctionDef.schema;
    const computedFieldTableRowArg = computedField.definition.table_argument;
    const computedFieldTableSessionArg =
      computedField.definition.session_argument;
    const computedFieldComment = getComputedFieldComment(computedField);

    let computedFieldFunction = null;
    let computedFieldFunctionDefinition = '';
    if (functions) {
      computedFieldFunction = dataSource.findFunction(
        functions,
        computedFieldFunctionName,
        computedFieldFunctionSchema
      );

      if (computedFieldFunction) {
        computedFieldFunctionDefinition = dataSource.getFunctionDefinition(
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
      saveFunc = (toggle: () => void) => {
        dispatch(
          saveComputedField(computedField, table, origComputedField, toggle)
        );
      };
    }

    const collapsedLabel = () => {
      if (isLast) {
        return null;
      }

      return (
        <div>
          <div className="flex items-center">
            <span
              className="font-semibold mr-xs"
              data-test={`computed-field-${origComputedFieldName}`}
            >
              {origComputedFieldName}
            </span>
            <span className="mr-xs">-</span>
            <span>{origComputedFieldFunctionName}</span>
          </div>
          <span key="comment" className="text-gray-600 text-sm">
            {origComputedFieldComment}
          </span>
        </div>
      );
    };

    // expand button text "View"
    const expandButtonText = isLast
      ? computedFields.length
        ? 'Add a new computed field'
        : 'Add a computed field'
      : 'Edit';

    const expandedLabel = () => {
      if (isLast) {
        return null;
      }

      return <b>{origComputedFieldName}</b>;
    };

    const expandedContent = () => {
      const getFunctionDefinitionSection = () => {
        if (!computedFieldFunctionName) return null;

        const modifyFunctionBtn = (
          <RawSqlButton
            dataTestId={`modify-function-${computedFieldFunctionName}`}
            sql={computedFieldFunctionDefinition}
            dispatch={dispatch}
            source={source}
          >
            Modify
          </RawSqlButton>
        );

        return (
          <div className="mb-md">
            <div className="flex items-center mb-formlabel">
              <h4 className="text-gray-600 font-semibold mr-auto">
                Function Definition:{' '}
              </h4>
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
              setOptions={{ useWorker: false }}
            />
          </div>
        );
      };

      const handleNameChange = (e: React.ChangeEvent<HTMLInputElement>) => {
        const newState = [...stateComputedFields];

        newState[i] = {
          ...newState[i],
          computed_field_name: e.target.value,
        };

        setComputedFieldsState(newState);
      };

      const handleFnSchemaChange = (
        selectedOption: string | any | null | undefined
      ) => {
        // fetch schema fn

        if (!selectedOption || typeof selectedOption === 'string') {
          return;
        }

        if (selectedOption) {
          dispatch(fetchFunctionInit(selectedOption.value));
        }

        const newState = [...stateComputedFields];

        newState[i] = {
          ...newState[i],
          definition: {
            ...newState[i].definition,
            function: {
              ...newState[i].definition.function,
              schema: selectedOption.value,
            },
          },
        };

        setComputedFieldsState(newState);
      };

      const handleFnNameChange = (
        selectedOption: string | any | null | undefined
      ) => {
        const newState = [...stateComputedFields];

        if (!selectedOption || typeof selectedOption === 'string') {
          return;
        }

        newState[i] = {
          ...newState[i],
          definition: {
            ...newState[i].definition,
            function: {
              ...newState[i].definition.function,
              name: selectedOption.value,
            },
          },
        };

        setComputedFieldsState(newState);
      };

      const handleCommentChange = (comment: string | null) => {
        const newState = [...stateComputedFields];

        newState[i] = {
          ...newState[i],
          comment,
        };

        setComputedFieldsState(newState);
      };

      const handleTableRowArgChange = (
        e: React.ChangeEvent<HTMLInputElement>
      ) => {
        const newState = [...stateComputedFields];
        newState[i] = {
          ...newState[i],
          definition: {
            ...newState[i].definition,
            table_argument: e.target.value || null,
          },
        };
        setComputedFieldsState(newState);
      };

      const handleTableSesssionArgChange = (
        e: React.ChangeEvent<HTMLInputElement>
      ) => {
        const newState = [...stateComputedFields];

        newState[i] = {
          ...newState[i],
          definition: {
            ...newState[i].definition,
            session_argument: e.target.value || null,
          },
        };

        setComputedFieldsState(newState);
      };

      const qualifiedFunctionName =
        computedFieldFunctionSchema === defaultSchemaForCurrentDataSource ||
        !computedFieldFunctionSchema
          ? `"${computedFieldFunctionName}"`
          : `"${computedFieldFunctionSchema}.${computedFieldFunctionName}"`;

      return (
        <div>
          <div className="mb-md">
            <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
              Computed Field Name:{' '}
            </h4>
            <input
              type="text"
              value={computedFieldName}
              onChange={handleNameChange}
              className={inputStyles}
              data-test="computed-field-name-input"
            />
          </div>

          <div className="mb-md">
            <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
              Function Schema:{' '}
            </h4>
            <div>
              <SearchableSelectBox
                options={schemaList}
                onChange={handleFnSchemaChange}
                value={computedFieldFunctionSchema}
                bsClass="function-schema-select"
                styleOverrides={{
                  menu: {
                    zIndex: 5,
                  },
                }}
                filterOption="prefix"
                placeholder="function_name"
              />
            </div>
          </div>

          <div className="mb-md">
            <div className="flex items-center mb-formlabel">
              <h4 className="flex items-center text-gray-600 font-semibold mr-auto">
                Function Name:{' '}
              </h4>
              <RawSqlButton
                dataTestId="create-function"
                sql=""
                dispatch={dispatch}
                source={source}
              >
                Create new
              </RawSqlButton>
            </div>
            <div data-test="functions-dropdown">
              <SearchableSelectBox
                options={dataSource
                  .getSchemaFunctions(
                    functions,
                    computedFieldFunctionSchema,
                    table.table_name,
                    table.table_schema
                  )
                  .map(fn => fn.function_name)}
                onChange={handleFnNameChange}
                value={computedFieldFunctionName}
                bsClass="function-name-select"
                styleOverrides={{
                  menu: {
                    zIndex: 25,
                  },
                }}
                filterOption="prefix"
                placeholder="function_name"
              />
            </div>
          </div>

          <div className="">{getFunctionDefinitionSection()}</div>

          <div className="mb-md">
            <div className="flex items-center mb-formlabel">
              <h4 className="text-gray-600 font-semibold">
                Table Row Argument:
              </h4>
              <Tooltip message="The argument of the function to which the table row is passed. By default, the first argument of the function is assumed to be the table row argument" />
            </div>
            <input
              type="text"
              value={computedFieldTableRowArg ?? undefined}
              placeholder="default: first argument"
              onChange={handleTableRowArgChange}
              className={inputStyles}
              data-test="computed-field-first-arg-input"
            />
          </div>

          <div className="mb-md">
            <div className="flex items-center mb-formlabel">
              <div className="flex items-center">
                <h4 className="text-gray-600 font-semibold">
                  Session Argument:
                </h4>
                <Tooltip message="The function argument into which Hasura session variables will be passed" />
              </div>
              <LearnMoreLink href="https://hasura.io/docs/latest/graphql/core/schema/computed-fields.html#accessing-hasura-session-variables-in-computed-fields" />
            </div>
            <input
              type="text"
              value={computedFieldTableSessionArg ?? ''}
              placeholder="hasura_session"
              onChange={handleTableSesssionArgChange}
              className={inputStyles}
            />
          </div>

          <div className="mb-md">
            <h4 className="flex items-center text-gray-600 font-semibold mb-formlabel">
              Comments:{' '}
            </h4>
            <CommentInput
              value={computedFieldComment}
              onChange={handleCommentChange}
              defaultComment={`A computed field, executes function ${qualifiedFunctionName}`}
              dataTest="computed-field-comment-input"
            />
          </div>
        </div>
      );
    };

    const resetComputedField = () => {
      const newState = [...stateComputedFields];

      newState[i] = {
        ...newState[i],
        ...(origComputedField || emptyComputedField),
      };

      setComputedFieldsState(newState);
    };

    const refreshFunctions = () => {
      if (computedFieldFunctionSchema) {
        dispatch(fetchFunctionInit(computedFieldFunctionSchema));
      }
    };

    return (
      <div key={`computed-field-${origComputedFieldName || i}`}>
        <ExpandableEditor
          editorExpanded={expandedContent}
          expandedLabel={expandedLabel}
          collapsedLabel={collapsedLabel}
          expandCallback={refreshFunctions}
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

  return <>{fieldsEditor}</>;
};

export default ComputedFieldsEditor;
