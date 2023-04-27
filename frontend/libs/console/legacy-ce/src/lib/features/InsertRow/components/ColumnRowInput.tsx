import { SupportedDrivers } from '../../hasura-metadata-types';
import { BooleanInput } from './BooleanInput';
import { getFormatDateFn } from './ColumnRowInput.utils';
import { DateInput } from './DateInput';
import { DateTimeInput } from './DateTimeInput';
import {
  ExpandableTextInput,
  ExpandableTextInputProps,
} from './ExpandableTextInput';
import { JsonInput } from './JsonInput';
import { TextInput } from './TextInput';
import { TimeInput } from './TimeInput';

type ColumnRowInputProps = ExpandableTextInputProps & {
  dataType: string;
  onValueChange: (e: { target: { value: string } }) => void;
  onCheckValueRadio: () => void;
  driver: SupportedDrivers;
};

export const ColumnRowInput: React.VFC<ColumnRowInputProps> = ({
  dataType,
  onValueChange,
  onCheckValueRadio,
  driver,
  ...props
}) => {
  if (dataType === 'string' || dataType === 'text') {
    return <ExpandableTextInput {...props} />;
  }

  if (dataType === 'date') {
    return <DateInput {...props} />;
  }

  if (
    dataType === 'datetime' ||
    dataType === 'timestamp without time zone' ||
    dataType === 'timestamp with time zone'
  ) {
    const formatDate = getFormatDateFn(dataType, driver);
    return <DateTimeInput {...props} formatDate={formatDate} />;
  }

  if (
    dataType === 'time' ||
    dataType === 'time without time zone' ||
    dataType === 'time with time zone'
  ) {
    const formatDate = getFormatDateFn(dataType, driver);
    return <TimeInput {...props} formatDate={formatDate} />;
  }

  if (dataType === 'jsondtype' || dataType === 'jsonb' || dataType === 'json') {
    return <JsonInput {...props} />;
  }

  if (dataType === 'boolean' || dataType === 'bool') {
    return (
      <BooleanInput
        checked={false}
        onCheckedChange={(isChecked: boolean) => {
          const value = isChecked ? 'true' : 'false';
          onValueChange({ target: { value } });
          onCheckValueRadio();
        }}
      />
    );
  }

  return <TextInput {...props} />;
};
