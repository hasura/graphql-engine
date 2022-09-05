import React from 'react';
import { useFieldArray, useFormContext, useWatch } from 'react-hook-form';

import { Button } from '@/new-components/Button';
import { CodeEditorField, InputField } from '@/new-components/Form';

interface Props {
  name: string;
  label: string;
  type: 'string' | 'number' | 'object';
}

export const BasicInput = ({ name, label, type }: Props) => {
  const { fields, append, remove } = useFieldArray({
    name: `basic-array-input-${name}`,
  });

  const { setValue } = useFormContext();

  const formValues: string[] = useWatch({ name });

  return (
    <div>
      <label className="font-semibold text-gray-600">{label}</label>
      <div className="bg-white p-6 border border-gray-300 rounded mb-6 max-w-xl">
        {fields.map((_, fieldIndex) => (
          <div className="flex justify-between">
            <div className="flex w-2/3">
              {type === 'object' ? (
                <CodeEditorField name={`${name}.${fieldIndex}`} label="" />
              ) : (
                <InputField
                  type={type === 'string' ? 'text' : 'number'}
                  name={`${name}.${fieldIndex}`}
                  label=""
                />
              )}
            </div>
            <div>
              <Button
                onClick={() => {
                  const value = formValues.filter((_x, i) => i !== fieldIndex);
                  remove(fieldIndex);
                  setValue(name, value);
                }}
              >
                Remove
              </Button>
            </div>
          </div>
        ))}
        <Button onClick={() => append('')}>Add</Button>
      </div>
    </div>
  );
};
