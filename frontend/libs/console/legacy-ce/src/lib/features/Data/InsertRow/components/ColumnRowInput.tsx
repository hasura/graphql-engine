import { TableColumn } from '@hasura/metadata/data-source';
import { ArrayInput } from './ArrayInput';
import { BooleanInput } from './BooleanInput';
import { getFormatDateFn, hasTimeZone } from './ColumnRowInput.utils';
import { DateInput } from './DateInput';
import { DateTimeInput } from './DateTimeInput';
import { EnumInput } from './EnumInput';
import {
  ExpandableTextInput,
  ExpandableTextInputProps,
} from './ExpandableTextInput';
import { JsonInput } from './JsonInput';
import { TimeInput } from './TimeInput';
import { TextInput } from './TextInput';

type ColumnRowInputProps = Omit<ExpandableTextInputProps, 'initialValue'> & {
  /** the generic data type, which picks the input */
  consoleDataType: TableColumn['consoleDataType'];
  /** the database type, which tells whether a date keeps a time zone */
  dataType: TableColumn['dataType'];
  /** the element data type of an array column */
  arrayItemType?: TableColumn['arrayItemType'];
  onValueChange: (e: { target: { value: unknown } }) => void;
  onCheckValueRadio: () => void;
  initialValue?: unknown;
  /** allowed values when the column references an enum table */
  enumValues?: string[];
};

export const ColumnRowInput: React.FC<ColumnRowInputProps> = ({
  consoleDataType,
  dataType,
  arrayItemType,
  onValueChange,
  onCheckValueRadio,
  initialValue,
  enumValues,
  ...props
}) => {
  if (enumValues) {
    return (
      <EnumInput
        {...props}
        values={enumValues}
        initialValue={initialValue}
        onValueChange={onValueChange}
        onCheckValueRadio={onCheckValueRadio}
      />
    );
  }

  switch (consoleDataType) {
    case 'array':
      return (
        <ArrayInput
          {...props}
          itemType={arrayItemType}
          initialValue={initialValue}
          onValueChange={onValueChange}
          onCheckValueRadio={onCheckValueRadio}
        />
      );

    case 'string':
    case 'text':
      return (
        <ExpandableTextInput
          {...props}
          initialValue={initialValue as string | undefined}
        />
      );

    case 'date':
      return (
        <DateInput {...props} defaultValue={initialValue as string | number} />
      );

    case 'timestamp':
      return (
        <DateTimeInput
          {...props}
          formatDate={getFormatDateFn('timestamp', hasTimeZone(dataType))}
          defaultValue={initialValue as string | number}
        />
      );

    case 'time':
      return (
        <TimeInput
          {...props}
          formatDate={getFormatDateFn('time', hasTimeZone(dataType))}
          defaultValue={initialValue as string | number}
        />
      );

    case 'json':
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

    case 'boolean':
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

    default:
      return (
        <TextInput {...props} defaultValue={initialValue as string | number} />
      );
  }
};
