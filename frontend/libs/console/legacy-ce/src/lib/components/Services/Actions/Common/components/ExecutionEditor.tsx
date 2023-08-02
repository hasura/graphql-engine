import React from 'react';

import { LearnMoreLink } from '../../../../../new-components/LearnMoreLink';
import { focusYellowRing } from '../../constants';
import { ActionExecution } from '../stateDefaults';

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
  label: 'Synchronous' | 'Asynchronous';
};

const ExecutionEditor: React.FC<ExecutionEditorProps> = ({
  value,
  onChange,
  disabled = false,
}) => {
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

  return (
    <>
      <h2 className="text-lg font-semibold mb-xs flex items-center">
        {editorLabel}
        <span className="text-red-700 ml-xs">*</span>
        <LearnMoreLink href={docsRef} className="font-normal" />
      </h2>

      {executionOptions.map((option, i) => (
        <div className="inline-flex items-center mr-md">
          <input
            key={i}
            id={option.value}
            name={option.value}
            type="radio"
            value={option.value}
            checked={value === option.value}
            onChange={() => onChange(option.value)}
            disabled={disabled}
            className={`mr-sm border-gray-400 ${focusYellowRing}`}
          />
          <label className="ml-sm pt-sm" htmlFor={option.value}>
            {option.label}
          </label>
        </div>
      ))}
    </>
  );
};

export default ExecutionEditor;
