import { SelectItem } from '../../../../../components/Common/SelectInputSplitField/SelectInputSplitField';
import { CreateBooleanMap } from '../../../../../components/Common/utils/tsUtils';
import { InputField, Select } from '../../../../../new-components/Form';
import { AddLogicalModelFormData } from '../validationSchema';
import { FieldsInput } from './FieldsInput';

export type LogicalModelFormProps = {
  sourceOptions: SelectItem[];
  typeOptions: string[];
  disabled?: CreateBooleanMap<AddLogicalModelFormData>;
};

export const LogicalModelFormInputs = (props: LogicalModelFormProps) => {
  return (
    <>
      <Select
        name="dataSourceName"
        label="Select a source"
        options={props.sourceOptions}
        dataTestId="dataSoureName"
        placeholder="Pick a database..."
        disabled={props.disabled?.dataSourceName}
      />
      <InputField
        dataTestId="name"
        name="name"
        label="Logical Model Name"
        placeholder="Enter a name for your logical Model"
        disabled={props.disabled?.name}
      />
      <FieldsInput
        name="fields"
        types={props.typeOptions}
        disabled={props.disabled?.fields}
      />
    </>
  );
};
