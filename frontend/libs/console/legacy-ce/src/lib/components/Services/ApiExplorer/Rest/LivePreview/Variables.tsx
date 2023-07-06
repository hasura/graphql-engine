import React, { ChangeEvent } from 'react';

import { VariableState } from './state';
import Input from './Input';
import PreviewTable from './PreviewTable';
import ToolTip from '../../../../Common/Tooltip/Tooltip';

type VariableComponentProps = {
  updateVariableValue: (
    name: string
  ) => (e: ChangeEvent<HTMLInputElement>) => void;
  variablesState: VariableState[];
};

const requestVariablesHeadings = [
  { content: 'Name', className: 'w-1/5 p-md text-center' },
  { content: 'Type', className: 'p-md text-center' },
  { content: 'Value' },
];

const Variables: React.FC<VariableComponentProps> = ({
  variablesState,
  updateVariableValue,
}) => {
  if (!variablesState || !variablesState.length) {
    return (
      <div className="w-full pt-sm flex justify-center items-center text-base font-bold text-center">
        This query doesn&apos;t require any request variables
      </div>
    );
  }

  return (
    <PreviewTable headings={requestVariablesHeadings}>
      {variablesState.map(v => (
        <tr className="border-b border-gray-300" key={`rest-var-${v.name}`}>
          <td className="">
            <b>{v.name}</b>
          </td>
          <td>
            <b>
              {v.type || v.kind}
              {v.kind === 'NonNullType' && '!'}
            </b>
            {v.kind === 'Unsupported' && (
              <ToolTip
                message="Complex types are not supported"
                placement="bottom"
              />
            )}
          </td>
          <td>
            <Input
              value={v.value}
              placeholder="Value..."
              onChange={updateVariableValue(v.name)}
              type="text"
              className="my-1 w-full"
            />
          </td>
        </tr>
      ))}
    </PreviewTable>
  );
};

export default Variables;
