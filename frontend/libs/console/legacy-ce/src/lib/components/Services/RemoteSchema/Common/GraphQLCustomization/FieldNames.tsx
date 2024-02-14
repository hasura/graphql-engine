import React, { useState, useEffect } from 'react';
import { IconTooltip } from '../../../../../new-components/Tooltip';
import { Button } from '../../../../../new-components/Button';
import TypeMapping from './TypeMapping';
import { inputStyles } from '../../constants';

type FieldNameType = {
  parentType?: string;
  prefix?: string | null;
  suffix?: string | null;
  mapping?: { type: string; custom_name: string }[];
};

type Props = {
  types: { typeName: string; fields: string[] }[];
  fieldName: FieldNameType;
  mode: 'edit' | 'create';
  onChange: (updateFieldName: FieldNameType) => void;
  onDelete?: () => void;
  onSave?: () => void;
  onClose?: () => void;
  label?: string;
};

const tooltip = (
  <IconTooltip
    message="Field remapping takes precedence to prefixes and suffixes."
    side="right"
  />
);

const SelectOne = ({
  options,
  value,
  onChange,
  label,
}: {
  options: string[];
  value: string | undefined;
  onChange: (e: any) => void;
  label?: string;
}) => (
  <select
    value={value}
    onChange={onChange}
    className={inputStyles}
    data-test={label}
    name={label}
  >
    <option value="">Select Type ...</option>
    {[...options].sort().map((op, i) => (
      <option value={op} key={i}>
        {op}
      </option>
    ))}
  </select>
);

const FieldNames = ({
  types,
  fieldName,
  mode,
  onChange,
  onDelete,
  onSave,
  onClose,
  label,
}: Props) => {
  const [fieldNameInput, setFieldNameInput] = useState<
    FieldNameType | undefined
  >(undefined);
  useEffect(() => {
    setFieldNameInput(fieldName);
  }, [fieldName]);

  return (
    <div className="border border-gray-300 p-md mt-xs">
      {mode === 'edit' ? null : (
        <div>
          <Button size="sm" onClick={onClose}>
            Close
          </Button>
        </div>
      )}
      <div className="flex items-center mt-md">
        <label className="w-1/3">Parent Type</label>
        <div className="w-2/3">
          <SelectOne
            options={types.map(v => v.typeName)}
            value={fieldNameInput?.parentType}
            onChange={e => {
              onChange({
                ...fieldNameInput,
                parentType: e.target.value,
              });
            }}
            label={label}
          />
        </div>
      </div>

      <div className="flex items-center mt-md">
        <label className="w-1/3">Field Prefix</label>
        <div className="w-2/3">
          <input
            type="text"
            className={inputStyles}
            placeholder="prefix_"
            value={fieldNameInput?.prefix || ''}
            onChange={e =>
              onChange({
                ...fieldNameInput,
                prefix: e.target.value || null,
              })
            }
            data-test={`remote-schema-customization-${label}-field-prefix-input`}
            name={`${label}-prefix`}
          />
        </div>
      </div>

      <div className="flex items-center mt-md">
        <label className="w-1/3">Field Suffix</label>
        <div className="w-2/3">
          <input
            type="text"
            className={inputStyles}
            placeholder="_suffix"
            value={fieldNameInput?.suffix || ''}
            onChange={e =>
              onChange({
                ...fieldNameInput,
                suffix: e.target.value || null,
              })
            }
            data-test={`remote-schema-customization-${label}-field-suffix-input`}
            name={`${label}-suffix`}
          />
        </div>
      </div>

      <div className="text-lg font-bold mt-md">
        Remap Field Names {tooltip}
        <TypeMapping
          types={
            types.find(x => x.typeName === fieldNameInput?.parentType)
              ?.fields || []
          }
          typeMappings={fieldNameInput?.mapping || []}
          onChange={updatedMaps =>
            onChange({
              ...fieldNameInput,
              mapping: updatedMaps,
            })
          }
          label={label}
        />
      </div>
      {mode === 'edit' ? (
        <div className="mt-md flex justify-end">
          <Button mode="destructive" size="sm" onClick={onDelete}>
            Remove
          </Button>
        </div>
      ) : (
        <div className="mt-md">
          <Button
            mode="primary"
            onClick={onSave}
            data-test="add-field-customization"
          >
            Add Field Customization
          </Button>
        </div>
      )}
    </div>
  );
};

export default FieldNames;
