import React, { ChangeEvent } from 'react';
import { VariableState } from './state';
import Input from './Input';
import PreviewTable from './PreviewTable';
import { IconTooltip, Table, Text } from '@hasura/shared/ui';

type VariableComponentProps = {
  updateVariableValue: (
    name: string,
  ) => (e: ChangeEvent<HTMLInputElement>) => void;
  variablesState: VariableState[];
};

const requestVariablesHeadings = [
  { content: 'Name', className: 'w-1/5' },
  { content: 'Type' },
  { content: 'Value' },
];

const Variables: React.FC<VariableComponentProps> = ({
  variablesState,
  updateVariableValue,
}) => {
  if (!variablesState || !variablesState.length) {
    return (
      <Text as="p" align="center" className="w-full py-4">
        This query doesn&apos;t require any request variables
      </Text>
    );
  }

  return (
    <PreviewTable headings={requestVariablesHeadings}>
      {variablesState.map((v) => (
        <Table.Row key={`rest-var-${v.name}`}>
          <Table.Cell>
            <Text>{v.name}</Text>
          </Table.Cell>
          <Table.Cell>
            <Text>
              {v.type || v.kind}
              {v.kind === 'NonNullType' && '!'}
            </Text>
            {v.kind === 'Unsupported' && (
              <IconTooltip
                message="Complex types are not supported"
                side="bottom"
              />
            )}
          </Table.Cell>
          <Table.Cell>
            <Input
              value={v.value}
              placeholder="Value..."
              onChange={updateVariableValue(v.name)}
              type="text"
            />
          </Table.Cell>
        </Table.Row>
      ))}
    </PreviewTable>
  );
};

export default Variables;
