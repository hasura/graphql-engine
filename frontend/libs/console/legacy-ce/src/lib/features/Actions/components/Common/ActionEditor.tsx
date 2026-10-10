import React from 'react';
import { GraphQLError } from 'graphql';
import {
  IconTooltip,
  DropdownButton,
  Badge,
  DropdownMenu,
  Input,
  FieldLabel,
  Text,
} from '@hasura/shared/ui';
import { Analytics, REDACT_EVERYTHING } from '@hasura/shared/analytics';
import { FaFileCode, FaMagic, FaTable } from 'react-icons/fa';
import HandlerEditor from './HandlerEditor';
import ExecutionEditor from './ExecutionEditor';
import HeaderConfEditor from './HeaderConfEditor';
import GraphQLEditor from './GraphQLEditor';
import GlobalTypesViewer from './GlobalTypesViewer';
import ActionDefIcon from '../../../../components/Common/Icons/ActionDef';
import TypesDefIcon from '../../../../components/Common/Icons/TypesDef';
import { TypeGeneratorModal } from './TypeGeneratorModal/TypeGeneratorModal';
import { ImportTypesModal } from './ImportTypesModal/ImportTypesModal';
import { ActionState, ActionExecution } from '../../types';
import { ClientHeader } from '@hasura/shared/types';
import { Flex, Heading } from '@radix-ui/themes';

const typeGeneratorMenuItemHeight = 'h-[72px]!';

type ActionEditorProps = ActionState & {
  readOnlyMode: boolean;
  actionType: string;
  commentOnChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  handlerOnChange: (v: string) => void;
  executionOnChange: (k: ActionExecution) => void;
  timeoutOnChange: (e: React.ChangeEvent<HTMLInputElement>) => void;
  setHeaders: (hs: ClientHeader[]) => void;
  toggleForwardClientHeaders: (value: boolean) => void;
  actionDefinitionOnChange: (
    value: string | null,
    error: GraphQLError | null,
    timer: NodeJS.Timeout | null,
    ast: Record<string, any> | null,
  ) => void;
  typeDefinitionOnChange: (
    value: string | null,
    error: GraphQLError | null,
    timer: NodeJS.Timeout | null,
    ast: Record<string, any> | null,
  ) => void;
};

const ActionEditor: React.FC<ActionEditorProps> = ({
  handler,
  kind,
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
      <Heading size="3">Action Configuration</Heading>
      <div className="my-4">
        <FieldLabel label="Comment / Description" />
        <Input
          className="mt-2"
          disabled={readOnlyMode}
          value={comment}
          placeholder="This is the comment which acts as a description for the action."
          onChange={commentOnChange}
          type="text"
          name="comment"
        />
      </div>

      <div className="mb-6">
        <Flex align="center" className=" mb-1" gap="2">
          <ActionDefIcon />
          Action Definition{' '}
          <Text color="red" size="1">
            *
          </Text>
        </Flex>
        <Text size="1">
          Define the action as a query or mutation using the GraphQL SDL.
        </Text>
        <div className="mb-2">
          <Text size="1">
            You can use the custom types already defined by you or define new
            types in the new types definition editor below.
          </Text>
        </div>
        <GraphQLEditor
          value={actionDefinitionSdl}
          error={actionDefinitionError ?? null}
          onChange={actionDefinitionOnChange}
          timer={actionParseTimer ?? null}
          readOnlyMode={readOnlyMode}
          width="100%"
          fontSize="12px"
        />
      </div>

      <Heading size="3">Type Configuration</Heading>

      <div className="mt-2 mb-6">
        <Flex gap="2">
          <div className="w-1/2">
            <FieldLabel
              labelIcon={<TypesDefIcon />}
              label="Declare New Types"
              description="You can define new GraphQL types which you can use in the action definition above."
            />
            <GraphQLEditor
              value={typesDefinitionSdl}
              error={typesDefinitionError ?? null}
              timer={typedefParseTimer ?? null}
              onChange={typeDefinitionOnChange}
              readOnlyMode={readOnlyMode}
              width="100%"
              height="224px"
              fontSize="12px"
              allowEmpty
            />
          </div>

          <div className="w-1/2">
            <GlobalTypesViewer />
          </div>
        </Flex>

        <TypeGeneratorModal
          isOpen={isTypesGeneratorOpen}
          onInsertTypes={(types) =>
            typeDefinitionOnChange(types, null, null, null)
          }
          onClose={() => setIsTypesGeneratorOpen(false)}
        />
        <ImportTypesModal
          isOpen={isImportTypesOpen}
          onInsertTypes={(types) =>
            typeDefinitionOnChange(types, null, null, null)
          }
          currentValue={typesDefinitionSdl}
          onClose={() => setIsImportTypesOpen(false)}
        />
        <div className="mt-4">
          <DropdownButton
            mode="default"
            leftIcon={FaMagic}
            items={[
              <Analytics
                key="actions-tab-btn-type-generator-from-json"
                name="actions-tab-btn-type-generator-from-json"
                passHtmlAttributesToChildren
              >
                <DropdownMenu.Item
                  className={typeGeneratorMenuItemHeight}
                  onSelect={() => setIsTypesGeneratorOpen(true)}
                >
                  <div>
                    <Flex align="center" gap="2">
                      <FaFileCode />
                      <Text as="p" weight="medium">
                        From JSON
                      </Text>
                    </Flex>
                    <Text as="div">
                      Generate GraphQL types from a JSON
                      <br />
                      response and request sample.
                    </Text>
                  </div>
                </DropdownMenu.Item>
              </Analytics>,
              <Analytics
                key="actions-tab-btn-type-generator-from-table"
                name="actions-tab-btn-type-generator-from-table"
                passHtmlAttributesToChildren
              >
                <DropdownMenu.Item
                  className={typeGeneratorMenuItemHeight}
                  onSelect={() =>
                    setTimeout(() => setIsImportTypesOpen(true), 0)
                  }
                >
                  <div>
                    <Flex align="center" gap="2">
                      <FaTable className="mr-1" />
                      <Text weight="medium">From Table</Text>
                      <Badge className="mx-2" color="blue">
                        BETA
                      </Badge>
                    </Flex>
                    <Text as="div">
                      Generate GraphQL types from the current
                      <br />
                      state of an existing tracked table.
                    </Text>
                  </div>
                </DropdownMenu.Item>
              </Analytics>,
            ]}
          >
            Type Generators
          </DropdownButton>
        </div>
      </div>

      <HandlerEditor
        value={handler}
        onChange={handlerOnChange}
        disabled={readOnlyMode}
      />

      <div className="mb-6 w-8/12">
        {actionType === 'query' ? null : (
          <ExecutionEditor
            value={kind}
            onChange={executionOnChange}
            disabled={readOnlyMode}
          />
        )}
      </div>

      <div className="mb-6 w-8/12">
        <HeaderConfEditor
          forwardClientHeaders={forwardClientHeaders}
          toggleForwardClientHeaders={toggleForwardClientHeaders}
          headers={headers}
          setHeaders={setHeaders}
          disabled={readOnlyMode}
        />
      </div>

      <Analytics name="ActionEditor" {...REDACT_EVERYTHING}>
        <div className="mb-6 w-8/12">
          <Flex align="center" gap="2" asChild>
            <Heading size="4">
              <Heading size="3">Action custom timeout</Heading>
              <IconTooltip message="Configure timeout for Action. Defaults to 30 seconds." />
            </Heading>
          </Flex>
          <div className="mb-6 w-4/12 mt-4">
            <Input
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
