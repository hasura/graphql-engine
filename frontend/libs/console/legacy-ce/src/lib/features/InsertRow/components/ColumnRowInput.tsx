import { dataSource } from '../../../dataSources';
import { BooleanInput } from './BooleanInput';
import { DateInput } from './DateInput';
import {
  ExpandableTextInput,
  ExpandableTextInputProps,
} from './ExpandableTextInput';
import { JsonInput } from './JsonInput';
import { TextInput } from './TextInput';

type ColumnRowInputProps = ExpandableTextInputProps & {
  dataType: string;
  onValueChange: (e: { target: { value: string } }) => void;
  onCheckValueRadio: () => void;
};

export const ColumnRowInput: React.VFC<ColumnRowInputProps> = ({
  dataType,
  onValueChange,
  onCheckValueRadio,
  ...props
}) => {
  if (dataType === dataSource.columnDataTypes.TEXT) {
    return <ExpandableTextInput {...props} />;
  }

  if (dataType === dataSource.columnDataTypes.DATE) {
    return <DateInput {...props} />;
  }

  if (
    dataType === dataSource.columnDataTypes.JSONDTYPE ||
    dataType === dataSource.columnDataTypes.JSONB
  ) {
    return <JsonInput {...props} />;
  }

  if (dataType === dataSource.columnDataTypes.BOOLEAN) {
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
