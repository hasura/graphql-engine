import { useFormContext } from 'react-hook-form';
import { FaLock } from 'react-icons/fa';
import { FiAlertTriangle } from 'react-icons/fi';
import { SelectItem } from '../../../../../components/Common/SelectInputSplitField/SelectInputSplitField';
import { CreateBooleanMap } from '../../../../../components/Common/utils/tsUtils';
import {
  GraphQLSanitizedInputField,
  Select,
} from '../../../../../new-components/Form';
import { IndicatorCard } from '../../../../../new-components/IndicatorCard';
import { LimitedFeatureWrapper } from '../../../../ConnectDBRedesign/components/LimitedFeatureWrapper/LimitedFeatureWrapper';

import { LogicalModel } from '../../../../hasura-metadata-types';
import { ReactQueryUIWrapper } from '../../../components';
import { useSupportedDataTypes } from '../../../hooks/useSupportedDataTypes';
import { AddLogicalModelFormData } from '../validationSchema';
import { FieldsInput } from './FieldsInput';

export type LogicalModelFormProps = {
  sourceOptions: SelectItem[];
  disabled?: CreateBooleanMap<AddLogicalModelFormData>;
  logicalModels: LogicalModel[];
  isThereBigQueryOrMssqlSource?: boolean;
  nameIsLocked?: boolean;
};

export const LogicalModelFormInputs = (props: LogicalModelFormProps) => {
  const { watch } = useFormContext<AddLogicalModelFormData>();

  const selectedDataSource = watch('dataSourceName');

  const supportedDataTypesReturn = useSupportedDataTypes({
    dataSourceName: selectedDataSource,
    options: {
      enabled: !!selectedDataSource,
    },
  });

  return (
    <>
      <Select
        name="dataSourceName"
        label="Select a source"
        options={props.sourceOptions}
        dataTestId="dataSourceName"
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
        placeholder="Enter a name for your Logical Model"
        icon={props?.nameIsLocked ? <FaLock /> : undefined}
        disabled={props.disabled?.name}
        hideTips
      />
      <ReactQueryUIWrapper
        useQueryResult={supportedDataTypesReturn}
        fallbackData={[]}
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
