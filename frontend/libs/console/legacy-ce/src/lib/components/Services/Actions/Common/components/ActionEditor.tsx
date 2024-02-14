/* eslint-disable no-underscore-dangle */
import React from 'react';
import { GraphQLError } from 'graphql';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import {
  Analytics,
  REDACT_EVERYTHING,
} from '../../../../../features/Analytics';
import { FaFileCode, FaMagic, FaTable } from 'react-icons/fa';
import { DropdownButton } from '../../../../../new-components/DropdownButton';
import { Badge } from '../../../../../new-components/Badge';
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
import { TypeGeneratorModal } from './TypeGeneratorModal/TypeGeneratorModal';
import { ImportTypesModal } from './ImportTypesModal/ImportTypesModal';

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

  const [isTypesGeneratorOpen, setIsTypesGeneratorOpen] = React.useState(false);
  const [isImportTypesOpen, setIsImportTypesOpen] = React.useState(false);

  return (
    <>
      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
        <h2 className="text-lg font-semibold mb-xs flex items-center">
          Action Configuration
        </h2>
      </Analytics>

      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
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
      </Analytics>

      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
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
      </Analytics>

      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
        <h2 className="text-lg font-semibold mb-xs flex items-center">
          Type Configuration
        </h2>
      </Analytics>

      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
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

          <TypeGeneratorModal
            isOpen={isTypesGeneratorOpen}
            onInsertTypes={types =>
              typeDefinitionOnChange(types, null, null, null)
            }
            onClose={() => setIsTypesGeneratorOpen(false)}
          />
          <ImportTypesModal
            isOpen={isImportTypesOpen}
            onInsertTypes={types =>
              typeDefinitionOnChange(types, null, null, null)
            }
            currentValue={typesDefinitionSdl}
            onClose={() => setIsImportTypesOpen(false)}
          />

          <DropdownButton
            items={[
              [
                <Analytics
                  name="actions-tab-btn-type-generator-from-json"
                  passHtmlAttributesToChildren
                >
                  <div
                    onClick={() =>
                      setTimeout(() => setIsTypesGeneratorOpen(true), 0)
                    }
                    className="py-xs font-semibold px-2.5 py-xs w-full"
                  >
                    <FaFileCode className="mr-xs" />
                    From JSON
                    <div className="text-muted font-normal">
                      Generate GraphQL types from a JSON
                      <p>response and request sample.</p>
                    </div>
                  </div>
                </Analytics>,
                <Analytics
                  name="actions-tab-btn-type-generator-from-table"
                  passHtmlAttributesToChildren
                >
                  <div
                    onClick={() =>
                      setTimeout(() => setIsImportTypesOpen(true), 0)
                    }
                    className="py-xs font-semibold px-2.5 w-96"
                  >
                    <FaTable className="mr-xs" />
                    From Table
                    <Badge className="mx-2" color="blue">
                      BETA
                    </Badge>
                    <div className="text-muted font-normal">
                      Generate GraphQL types from the current
                      <p>state of an existing tracked table.</p>
                    </div>
                  </div>
                </Analytics>,
              ],
            ]}
          >
            <FaMagic className="mr-xs" />
            Type Generators
          </DropdownButton>
        </div>
      </Analytics>

      <HandlerEditor
        value={handler}
        onChange={handlerOnChange}
        disabled={readOnlyMode}
      />

      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
        <div className="mb-lg w-8/12">
          {actionType === 'query' ? null : (
            <ExecutionEditor
              value={execution}
              onChange={executionOnChange}
              disabled={readOnlyMode}
            />
          )}
        </div>
      </Analytics>

      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
        <div className="mb-lg w-8/12">
          <HeaderConfEditor
            forwardClientHeaders={forwardClientHeaders}
            toggleForwardClientHeaders={toggleForwardClientHeaders}
            headers={headers}
            setHeaders={setHeaders}
            disabled={readOnlyMode}
          />
        </div>
      </Analytics>

      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
        <div className="mb-lg w-8/12">
          <h2 className="text-lg font-semibold mb-xs flex items-center">
            Action custom timeout
            <IconTooltip message="Configure timeout for Action. Defaults to 30 seconds." />
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
      </Analytics>
    </>
  );
};

export default ActionEditor;
