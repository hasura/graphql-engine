import { SupportedDriver } from '@hasura/shared/types';
import { BooleanInput } from './BooleanInput';
import { getFormatDateFn } from './ColumnRowInput.utils';
import { DateInput } from './DateInput';
import { DateTimeInput } from './DateTimeInput';
import {
  ExpandableTextInput,
  ExpandableTextInputProps,
} from './ExpandableTextInput';
import { JsonInput } from './JsonInput';
import { TimeInput } from './TimeInput';
import { TextInput } from './TextInput';

type ColumnRowInputProps = Omit<ExpandableTextInputProps, 'initialValue'> & {
  dataType: string;
  onValueChange: (e: { target: { value: string } }) => void;
  onCheckValueRadio: () => void;
  driver: SupportedDriver;
  initialValue?: unknown;
};

export const ColumnRowInput: React.FC<ColumnRowInputProps> = ({
  dataType,
  onValueChange,
  onCheckValueRadio,
  driver,
  initialValue,
  ...props
}) => {
  if (dataType === 'string' || dataType === 'text') {
    return (
      <ExpandableTextInput
        {...props}
        initialValue={initialValue as string | undefined}
      />
    );
  }

  if (dataType === 'date') {
    return (
      <DateInput {...props} defaultValue={initialValue as string | number} />
    );
  }

  if (
    dataType === 'datetime' ||
    dataType === 'timestamp without time zone' ||
    dataType === 'timestamp with time zone'
  ) {
    const formatDate = getFormatDateFn(dataType, driver);
    return (
      <DateTimeInput
        {...props}
        formatDate={formatDate}
        defaultValue={initialValue as string | number}
      />
    );
  }

  if (
    dataType === 'time' ||
    dataType === 'time without time zone' ||
    dataType === 'time with time zone'
  ) {
    const formatDate = getFormatDateFn(dataType, driver);
    return (
      <TimeInput
        {...props}
        formatDate={formatDate}
        defaultValue={initialValue as string | number}
      />
    );
  }

  if (dataType === 'jsondtype' || dataType === 'jsonb' || dataType === 'json') {
    return (
      <JsonInput
        {...props}
        initialValue={
          typeof initialValue === 'string'
            ? initialValue
            : initialValue !== undefined
              ? JSON.stringify(initialValue)
              : undefined
        }
      />
    );
  }

  if (dataType === 'boolean' || dataType === 'bool') {
    return (
      <BooleanInput
        checked={false}
        initialValue={
          typeof initialValue === 'boolean' ? initialValue : undefined
        }
        onCheckedChange={(isChecked: boolean) => {
          const value = isChecked ? 'true' : 'false';
          onValueChange({ target: { value } });
          onCheckValueRadio();
        }}
      />
    );
  }

  return (
    <TextInput {...props} defaultValue={initialValue as string | number} />
  );
};
