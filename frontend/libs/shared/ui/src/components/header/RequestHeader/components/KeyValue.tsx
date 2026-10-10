import { RiCloseCircleFill } from 'react-icons/ri';
import { Flex, Grid } from '@radix-ui/themes';
import { InputField } from '../../../Form';
import { IconButton } from '../../../Button';

interface Props {
  fieldId: string;
  fieldName: string;
  rowIndex: number;
  removeRow: (index?: number | number[]) => void;
}

export const KeyValue = ({ fieldName, rowIndex, removeRow }: Props) => {
  const keyLabel = `${fieldName}[${rowIndex}].name`;
  const valueLabel = `${fieldName}[${rowIndex}].value`;

  return (
    <Grid columns="2" gap="2" className="mb-4">
      <InputField
        name={keyLabel}
        aria-label={keyLabel}
        data-test={`header-test${rowIndex}-key`}
        noErrorPlaceholder
        fieldProps={{
          placeholder: 'Key',
        }}
      />
      <Flex align="center" justify="between" gap="2">
        <InputField
          name={valueLabel}
          aria-label={valueLabel}
          data-test={`header-test${rowIndex}-value`}
          noErrorPlaceholder
          fieldProps={{
            placeholder: 'Value or {{Environment_Variable}}',
          }}
        />
        <IconButton
          variant="ghost"
          radius="full"
          data-test={`delete-header-${rowIndex}`}
          onClick={() => {
            removeRow(rowIndex);
          }}
        >
          <RiCloseCircleFill />
        </IconButton>
      </Flex>
    </Grid>
  );
};
