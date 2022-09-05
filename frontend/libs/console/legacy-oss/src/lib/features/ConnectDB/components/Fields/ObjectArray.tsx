import React from 'react';
import { useFieldArray, useFormContext, useWatch } from 'react-hook-form';

import { OneOf, Property, Ref } from '@/features/DataSource';
import { Button } from '@/new-components/Button';
import { RenderProperty } from './RenderProperty';

interface Props {
  items: {
    type: 'object';
    properties: Record<string, Ref | Property | OneOf>;
  };
  name: string;
  otherSchemas: Record<string, Property>;
  label: string;
}

export const ObjectArray = ({ items, name, otherSchemas, label }: Props) => {
  const { fields, append, remove } = useFieldArray({
    name: `${name}-object-array-input`,
  });

  const { setValue } = useFormContext();

  const formValues: Record<string, any>[] = useWatch({ name });

  return (
    <div>
      <label className="font-semibold text-gray-600">{label}</label>
      <div className="rounded space-y-4 mb-6 max-w-xl">
        {fields.map((_, index) => (
          <div className="bg-white p-6 border border-gray-300 rounded space-y-4 mb-6 max-w-xl">
            <RenderProperty
              property={items}
              otherSchemas={otherSchemas}
              name={`${name}.${index}`}
            />
            <div>
              <Button
                onClick={() => {
                  remove(index);
                  setValue(
                    name,
                    formValues.filter((_x, i) => i !== index)
                  );
                }}
              >
                Remove
              </Button>
            </div>
          </div>
        ))}
        <div>
          <Button onClick={() => append('')}>Add</Button>
        </div>
      </div>
    </div>
  );
};
