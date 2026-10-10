import { FaLock } from 'react-icons/fa';
import { FiAlertTriangle } from 'react-icons/fi';
import {
  GraphQLSanitizedInputField,
  IndicatorCard,
  SelectField,
  SelectItemProps,
} from '@hasura/shared/ui';
import { LimitedFeatureWrapper } from '../../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';
import { LogicalModel, Source } from '@hasura/shared/types';
import { ReactQueryUIWrapper } from '../../../components';
import { AddLogicalModelFormData } from '../validationSchema';
import { FieldsInput } from './FieldsInput';
import { CreateBooleanMap } from '@hasura/shared/types';
import { useSupportedScalars } from '@hasura/metadata/data-source';

export type LogicalModelFormProps = {
  source: Source | undefined;
  sourceOptions: SelectItemProps[];
  disabled?: CreateBooleanMap<AddLogicalModelFormData>;
  logicalModels: LogicalModel[];
  isThereBigQueryOrMssqlSource?: boolean;
  nameIsLocked?: boolean;
};

export const LogicalModelFormInputs = (props: LogicalModelFormProps) => {
  const supportedDataTypesReturn = useSupportedScalars(props.source?.kind);

  return (
    <>
      <SelectField
        name="dataSourceName"
        label="Select a source"
        options={props.sourceOptions}
        dataTestId="dataSourceName"
        placeholder="Pick a database..."
        disabled={props.disabled?.dataSourceName}
      />
      <div className="max-w-4xl my-2">
        {props.isThereBigQueryOrMssqlSource && (
          <LimitedFeatureWrapper
            title="Looking to add Logical Models for SQL Server/Big Query databases?"
            id="native-queries"
            description="Get production-ready today with a 30-day free trial of Hasura EE, no credit card required."
          />
        )}
      </div>
      {props?.nameIsLocked && (
        <IndicatorCard
          showIcon
          headline="Name is locked"
          customIcon={() => <FiAlertTriangle />}
          status="info"
        >
          The Name field cannot be changed because this Logical Model is
          referenced by a Table.
        </IndicatorCard>
      )}
      <GraphQLSanitizedInputField
        dataTestId="name"
        name="name"
        label="Logical Model Name"
        hideTips
        fieldProps={{
          placeholder: 'Enter a name for your Logical Model',
          icon: props?.nameIsLocked ? FaLock : undefined,
          disabled: props.disabled?.name,
        }}
      />
      <ReactQueryUIWrapper
        useQueryResult={supportedDataTypesReturn}
        fallbackData={[] as string[]}
        loadingStyle="overlay"
        loader="spinner"
        miniSpinnerBackdrop
        render={({ data: typeOptions }) => (
          <FieldsInput
            name="fields"
            types={typeOptions}
            disabled={props.disabled?.fields}
            logicalModels={props.logicalModels}
          />
        )}
      />
    </>
  );
};
