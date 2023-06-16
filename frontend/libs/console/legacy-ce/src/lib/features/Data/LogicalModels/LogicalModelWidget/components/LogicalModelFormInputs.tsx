import { SelectItem } from '../../../../../components/Common/SelectInputSplitField/SelectInputSplitField';
import { CreateBooleanMap } from '../../../../../components/Common/utils/tsUtils';
import {
  GraphQLSanitizedInputField,
  Select,
} from '../../../../../new-components/Form';
import { LogicalModel } from '../../../../hasura-metadata-types';
import { AddLogicalModelFormData } from '../validationSchema';
import { FieldsInput } from './FieldsInput';

export type LogicalModelFormProps = {
  sourceOptions: SelectItem[];
  typeOptions: string[];
  disabled?: CreateBooleanMap<AddLogicalModelFormData>;
  logicalModels: LogicalModel[];
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
      <GraphQLSanitizedInputField
        dataTestId="name"
        name="name"
        label="Logical Model Name"
        placeholder="Enter a name for your logical Model"
        disabled={props.disabled?.name}
        hideTips
      />
      <FieldsInput
        name="fields"
        types={props.typeOptions}
        disabled={props.disabled?.fields}
        logicalModels={props.logicalModels}
      />
    </>
  );
};
