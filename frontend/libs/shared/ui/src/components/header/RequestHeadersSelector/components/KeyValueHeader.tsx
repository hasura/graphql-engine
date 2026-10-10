import { RiCloseCircleFill } from 'react-icons/ri';
import { Table } from '../../../table/Table';
import { InputField, SelectField } from '../../../Form';
import { Flex } from '@radix-ui/themes';
import { IconButton } from '../../../Button';

interface Props {
  fieldName: string;
  rowIndex: number;
  typeSelect: boolean;
  removeRow: (index?: number | number[]) => void;
  disabled?: boolean;
}

const HEADER_TYPE_OPTIONS = [
  {
    value: 'value',
    label: 'Value',
  },
  {
    value: 'env',
    label: 'Env Var',
  },
];

export const KeyValueHeader = ({
  fieldName,
  rowIndex,
  typeSelect,
  removeRow,
  disabled,
}: Props) => {
  const keyLabel = `${fieldName}[${rowIndex}].name`;
  const typeLabel = `${fieldName}[${rowIndex}].type`;
  const valueLabel = `${fieldName}[${rowIndex}].value`;

  return (
    <Table.Row>
      <Table.Cell>
        <InputField
          name={keyLabel}
          aria-label={keyLabel}
          data-test={`header-test${rowIndex}-key`}
          fieldProps={{
            disabled,
            placeholder: 'Key...',
          }}
          noErrorPlaceholder
        />
      </Table.Cell>
      {typeSelect ? (
        <Table.Cell>
          <SelectField
            name={typeLabel}
            aria-label={typeLabel}
            disabled={disabled}
            placeholder="Select..."
            noErrorPlaceholder
            options={HEADER_TYPE_OPTIONS}
          />
        </Table.Cell>
      ) : null}
      <Table.Cell>
        <Flex align="start" gap="2">
          <InputField
            name={valueLabel}
            aria-label={valueLabel}
            data-test={`header-test${rowIndex}-value`}
            noErrorPlaceholder
            fieldProps={{
              placeholder: 'Value...',
              disabled,
            }}
          />
          <div className="mt-2">
            <IconButton
              variant="ghost"
              radius="full"
              disabled={disabled}
              onClick={() => {
                if (disabled) {
                  return;
                }

                removeRow(rowIndex);
              }}
              data-test={`delete-header-${rowIndex}`}
            >
              <RiCloseCircleFill className="w-4 h-4" />
            </IconButton>
          </div>
        </Flex>
      </Table.Cell>
    </Table.Row>
  );
};
