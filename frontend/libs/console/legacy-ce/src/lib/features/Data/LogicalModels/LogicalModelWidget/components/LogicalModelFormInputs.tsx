import { SelectItem } from '../../../../../components/Common/SelectInputSplitField/SelectInputSplitField';
import { CreateBooleanMap } from '../../../../../components/Common/utils/tsUtils';
import {
  GraphQLSanitizedInputField,
  Select,
} from '../../../../../new-components/Form';
import { LimitedFeatureWrapper } from '../../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import { LogicalModel } from '../../../../hasura-metadata-types';
import { AddLogicalModelFormData } from '../validationSchema';
import { FieldsInput } from './FieldsInput';

export type LogicalModelFormProps = {
  sourceOptions: SelectItem[];
  typeOptions: string[];
  disabled?: CreateBooleanMap<AddLogicalModelFormData>;
  logicalModels: LogicalModel[];
  isThereBigQueryOrMssqlSource?: boolean;
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
      <div className="max-w-4xl">
        {props.isThereBigQueryOrMssqlSource && (
          <LimitedFeatureWrapper
            title="Looking to add Logical Models for SQL Server/Big Query databases?"
            id="native-queries"
            description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
          />
        )}
      </div>
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
