import React from 'react';
import { GraphQLError } from 'graphql';
import { Tooltip, OverlayTrigger } from 'react-bootstrap';
import { FaQuestionCircle } from 'react-icons/fa';
import HandlerEditor from './HandlerEditor';
import ExecutionEditor from './ExecutionEditor';
import HeaderConfEditor from './HeaderConfEditor';
import GraphQLEditor from './GraphQLEditor';
import GlobalTypesViewer from './GlobalTypesViewer';
import { ActionExecution, Definition, Header } from '../stateDefaults';
import { Nullable } from '../../../../Common/utils/tsUtils';
import ActionDefIcon from '../../../../Common/Icons/ActionDef';
import TypesDefIcon from '../../../../Common/Icons/TypesDef';
import { inputStyles } from '../../constants';

type ActionEditorProps = {
  handler: string;
  execution: ActionExecution;
  actionDefinition: Definition;
  typeDefinition: Definition;
  headers: Header[];
  forwardClientHeaders: boolean;
  readOnlyMode: boolean;
  timeout: string;
  comment: string;
  actionType: string;
  commentOnChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  handlerOnChange: (v: string) => void;
  executionOnChange: (k: ActionExecution) => void;
  timeoutOnChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  setHeaders: (hs: Header[]) => void;
  toggleForwardClientHeaders: () => void;
  actionDefinitionOnChange: (
    value: Nullable<string>,
    error: Nullable<GraphQLError>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => void;
  typeDefinitionOnChange: (
    value: Nullable<string>,
    error: Nullable<GraphQLError>,
    timer: Nullable<NodeJS.Timeout>,
    ast: Nullable<Record<string, any>>
  ) => void;
};

const ActionEditor: React.FC<ActionEditorProps> = ({
  handler,
  execution,
  actionDefinition,
  typeDefinition,
  headers,
  forwardClientHeaders,
  readOnlyMode,
  timeout,
  comment,
  actionType,
  commentOnChange,
  handlerOnChange,
  executionOnChange,
  timeoutOnChange,
  setHeaders,
  toggleForwardClientHeaders,
  actionDefinitionOnChange,
  typeDefinitionOnChange,
}) => {
  const {
    sdl: typesDefinitionSdl,
    error: typesDefinitionError,
    timer: typedefParseTimer,
  } = typeDefinition;

  const {
    sdl: actionDefinitionSdl,
    error: actionDefinitionError,
    timer: actionParseTimer,
  } = actionDefinition;

  return (
    <>
      <h2 className="text-lg font-semibold mb-xs flex items-center">
        Action Configuration
      </h2>

      <div className="mb-md w-8/12">
        <label
          htmlFor="comment"
          className="block text-gray-600 font-medium mb-xs"
        >
          Comment / Description
        </label>
        <input
          disabled={readOnlyMode}
          value={comment}
          placeholder="This is the comment which acts as a description for the action."
          onChange={commentOnChange}
          type="text"
          name="comment"
          className={inputStyles}
        />
      </div>

      <div className="mb-lg">
        <label
          htmlFor="action"
          className="flex items-center block text-gray-600 font-medium mb-xs"
        >
          <ActionDefIcon />
          Action Definition <span className="ml-xs text-red-700">*</span>
        </label>
        <p className="text-sm text-gray-600">
          Define the action as a query or mutation using the GraphQL SDL.
        </p>
        <p className="text-sm text-gray-600 mb-sm">
          You can use the custom types already defined by you or define new
          types in the new types definition editor below.
        </p>
        <GraphQLEditor
          value={actionDefinitionSdl}
          error={actionDefinitionError}
          onChange={actionDefinitionOnChange}
          timer={actionParseTimer}
          readOnlyMode={readOnlyMode}
          width="100%"
          fontSize="12px"
        />
      </div>

      <h2 className="text-lg font-semibold mb-xs flex items-center">
        Type Configuration
      </h2>

      <div className="mb-lg">
        <div className="grid gap-4 grid-cols-2 mb-sm">
          <div>
            <label
              htmlFor="types"
              className="flex items-center block text-gray-600 font-medium mb-xs"
            >
              <TypesDefIcon />
              Declare New Types
            </label>
            <p className="text-sm text-gray-600 mb-sm">
              You can define new GraphQL types which you can use in the action
              definition above.
            </p>
            <GraphQLEditor
              value={typesDefinitionSdl}
              error={typesDefinitionError}
              timer={typedefParseTimer}
              onChange={typeDefinitionOnChange}
              readOnlyMode={readOnlyMode}
              width="100%"
              fontSize="12px"
              allowEmpty
            />
          </div>

          <GlobalTypesViewer />
        </div>
      </div>

      <HandlerEditor
        value={handler}
        onChange={handlerOnChange}
        disabled={readOnlyMode}
      />

      <div className="mb-lg w-8/12">
        {actionType === 'query' ? null : (
          <ExecutionEditor
            value={execution}
            onChange={executionOnChange}
            disabled={readOnlyMode}
          />
        )}
      </div>

      <div className="mb-lg w-8/12">
        <HeaderConfEditor
          forwardClientHeaders={forwardClientHeaders}
          toggleForwardClientHeaders={toggleForwardClientHeaders}
          headers={headers}
          setHeaders={setHeaders}
          disabled={readOnlyMode}
        />
      </div>

      <div className="mb-lg w-8/12">
        <h2 className="text-lg font-semibold mb-xs flex items-center">
          Action custom timeout
          <OverlayTrigger
            placement="right"
            overlay={
              <Tooltip id="tooltip-cascade">
                Configure timeout for Action. Defaults to 30 seconds.
              </Tooltip>
            }
          >
            <FaQuestionCircle className="ml-xs" aria-hidden="true" />
          </OverlayTrigger>
        </h2>
        <div className="mb-lg w-4/12">
          <input
            className={`${inputStyles} mb-sm`}
            type="number"
            placeholder="Timeout in seconds"
            value={timeout}
            data-key="timeoutConf"
            data-test="action-timeout-seconds"
            onChange={timeoutOnChange}
            disabled={readOnlyMode}
            pattern="^\d+$"
            title="Only non negative integers are allowed"
          />
        </div>
      </div>
    </>
  );
};

export default ActionEditor;
