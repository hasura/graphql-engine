import React from 'react';
import { useFormContext } from 'react-hook-form';
import { FaKey, FaPlug } from 'react-icons/fa';
import { FiType } from 'react-icons/fi';
import { InputField, SelectField } from '../../../../../new-components/Form';
import { RsToRsSchema } from '../../types';

export interface RsSourceTypeSelectorProps {
  types: string[];
  sourceTypeKey: string;
  nameTypeKey: string;
  isModify: boolean;
}

export const remoteSchemaSelectorKey = 'sourceRemoteSchema';

export const RsSourceTypeSelector = ({
  types,
  sourceTypeKey,
  nameTypeKey,
  isModify,
}: RsSourceTypeSelectorProps) => {
  const { watch } = useFormContext<RsToRsSchema>();
  const remoteSchemaName = watch(remoteSchemaSelectorKey);
  const typeOptions = React.useMemo(
    () => types.map(t => ({ value: t, label: t })),
    [types]
  );
  const schemaOptions = [{ value: remoteSchemaName, label: remoteSchemaName }];

  return (
    <div className="bg-gray-50 rounded p-md border border-gray-300 border-l-4 border-l-green-600 w-full h-full">
      <div className="mb-sm w-full">
        <SelectField
          label="Source Remote Schema"
          name={remoteSchemaSelectorKey}
          placeholder="Select a remote schema"
          options={schemaOptions}
          labelIcon={<FaPlug />}
          disabled
        />
      </div>
      <div className="mb-sm w-full">
        <SelectField
          label="Source Type"
          name={sourceTypeKey}
          placeholder="Select a type"
          options={typeOptions}
          labelIcon={<FiType style={{ strokeWidth: 4.5 }} />}
          dataTest="select-source-type"
        />
      </div>
      <div className="mb-sm w-full">
        <InputField
          name={nameTypeKey}
          label="Relationship Name"
          description="This will be used as the field name in the source type."
          placeholder="Relationship name"
          dataTest="rs-to-rs-rel-name"
          disabled={isModify}
          labelIcon={<FaKey style={{ strokeWidth: 4.5 }} />}
        />
      </div>
    </div>
  );
};
