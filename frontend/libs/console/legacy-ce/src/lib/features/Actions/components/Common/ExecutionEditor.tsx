import React from 'react';
import { LearnMoreLink, RadioGroup, Text } from '@hasura/shared/ui';
import { ActionExecution } from '../../types';
import { Flex, Heading } from '@radix-ui/themes';

const editorLabel = 'Execution';
const docsRef =
  'https://docs.hasura.io/1.0/graphql/manual/actions/async-actions.html';

type ExecutionEditorProps = {
  value: string;
  onChange: (data: ActionExecution) => void;
  disabled?: boolean;
};

type ExecutionOptions = {
  value: 'synchronous' | 'asynchronous';
  label: string;
};

const executionOptions: ExecutionOptions[] = [
  {
    value: 'synchronous',
    label: 'Synchronous',
  },
  {
    value: 'asynchronous',
    label: 'Asynchronous',
  },
];

const ExecutionEditor: React.FC<ExecutionEditorProps> = ({
  value,
  onChange,
  disabled = false,
}) => {
  return (
    <>
      <Flex gap="2" align="center" className="mb-4">
        <Heading
          size="4"
          className="text-lg font-semibold mb-1 flex items-center"
        >
          <Text>{editorLabel}</Text>
          <Text color="red">*</Text>
          <LearnMoreLink href={docsRef} className="font-normal" />
        </Heading>
      </Flex>
      <RadioGroup
        orientation="horizontal"
        value={value}
        disabled={disabled}
        onChange={(value) => onChange(value as ActionExecution)}
        options={executionOptions.map((option, i) => ({
          label: option.label,
          value: option.value,
        }))}
      />
    </>
  );
};

export default ExecutionEditor;
