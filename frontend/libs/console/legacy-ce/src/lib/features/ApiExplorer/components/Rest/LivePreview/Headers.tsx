import React, { ChangeEvent } from 'react';
import { FaTimesCircle } from 'react-icons/fa';
import { Checkbox } from '@radix-ui/themes';
import { HeaderState } from './state';
import Input from './Input';
import PreviewTable from './PreviewTable';
import { IconButton, Table, Text } from '@hasura/shared/ui';

type UpdateHeaderValues = (
  index: number,
) => (e: ChangeEvent<HTMLInputElement>) => void;

type HeaderComponentProps = {
  updateKeyText: UpdateHeaderValues;
  updateValueText: UpdateHeaderValues;
  toggleActiveState: (index: number) => (checked: boolean) => void;
  onClickRemove: (index: number) => () => void;
  headerState: HeaderState[];
};

const requestHeadersHeadings = [
  {
    content: '',
    className: 'w-2/12',
  },
  { content: 'Key' },
  { content: 'Value' },
  { content: '' },
];

const Headers: React.FC<HeaderComponentProps> = ({
  headerState,
  updateKeyText,
  updateValueText,
  toggleActiveState,
  onClickRemove,
}) => {
  if (!headerState || !headerState.length) {
    return (
      <div className="w-full py-4">
        <Text as="p" align="center">
          Click on the &apos;Add Header&apos; to add some Request Headers
        </Text>
      </div>
    );
  }

  return (
    <PreviewTable headings={requestHeadersHeadings}>
      {headerState.map((header) => (
        <Table.Row key={`rest-header-${header.index}`}>
          <Table.Cell className="p-4">
            <Checkbox
              value={header.key}
              onCheckedChange={toggleActiveState(header.index)}
              checked={header.selected}
            />
          </Table.Cell>
          <Table.Cell>
            <Input
              value={header.key}
              onChange={updateKeyText(header.index)}
              placeholder="Key..."
            />
          </Table.Cell>
          <Table.Cell>
            {header.key === 'admin-secret' ? (
              <Input
                value={header.value}
                onChange={updateValueText(header.index)}
                placeholder="Value..."
                type="password"
              />
            ) : (
              <Input
                value={header.value}
                onChange={updateValueText(header.index)}
                placeholder="Value..."
                type="text"
              />
            )}
          </Table.Cell>
          <Table.Cell>
            <IconButton
              color="indigo"
              variant="ghost"
              radius="full"
              onClick={onClickRemove(header.index)}
            >
              <FaTimesCircle />
            </IconButton>
          </Table.Cell>
        </Table.Row>
      ))}
    </PreviewTable>
  );
};

export default Headers;
