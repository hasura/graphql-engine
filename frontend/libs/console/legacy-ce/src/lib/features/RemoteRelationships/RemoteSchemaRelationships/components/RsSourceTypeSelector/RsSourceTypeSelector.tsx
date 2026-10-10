import { FaKey, FaPlug } from 'react-icons/fa';
import { FiType } from 'react-icons/fi';
import {
  Card,
  createTextOption,
  FieldLabel,
  InputField,
  ReactSelectField,
  Select,
} from '@hasura/shared/ui';
import { createFilter } from 'react-select';

export interface RsSourceTypeSelectorProps {
  types: string[];
  remoteSchemaName: string;
  sourceTypeKey: string;
  nameTypeKey: string;
  isModify: boolean;
  disabled?: boolean;
}

const filterOption = createFilter({
  ignoreCase: true,
  matchFrom: 'any',
});
export const RsSourceTypeSelector = ({
  types,
  sourceTypeKey,
  nameTypeKey,
  isModify,
  remoteSchemaName,
  disabled,
}: RsSourceTypeSelectorProps) => {
  const typeOptions = types.map(createTextOption);

  return (
    <Card className="border-l-4 border-l-green-600 w-full h-full">
      <div className="mb-2 w-full">
        <FieldLabel
          labelIcon={FaPlug}
          label="Source Remote Schema"
          className="mb-2"
        />
        <Select
          placeholder="Select a remote schema"
          options={[createTextOption(remoteSchemaName)]}
          value={remoteSchemaName}
          full
          disabled
        />
      </div>
      <div className="mb-2 w-full">
        <ReactSelectField
          label="Source Type"
          name={sourceTypeKey}
          placeholder="Select a type"
          options={typeOptions}
          labelIcon={FiType}
          dataTest="select-source-type"
          disabled={isModify || disabled}
          noErrorPlaceholder
          selectProps={{
            isSearchable: true,
            filterOption,
          }}
        />
      </div>
      <div className="mb-2 w-full">
        <InputField
          name={nameTypeKey}
          label="Relationship Name"
          description="This will be used as the field name in the source type."
          dataTest="rs-to-rs-rel-name"
          labelIcon={FaKey}
          noErrorPlaceholder
          fieldProps={{
            placeholder: 'Relationship name',
            disabled: isModify || disabled,
          }}
        />
      </div>
    </Card>
  );
};
