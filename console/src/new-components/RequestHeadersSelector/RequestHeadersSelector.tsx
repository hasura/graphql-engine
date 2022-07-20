import React from 'react';
import { useFieldArray } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '@/new-components/Button';
import { RequestHeadersSelectorSchema } from './schema';
import { KeyValueHeader } from './components/KeyValueHeader';

export interface RequestHeadersSelectorProps {
  name: string;
}

export const RequestHeadersSelector = (props: RequestHeadersSelectorProps) => {
  const { name } = props;
  const { fields, append, remove } = useFieldArray<
    Record<string, RequestHeadersSelectorSchema>
  >({
    name,
  });
  const thereIsAtLeastOneField = fields.length > 0;

  return (
    <div>
      {thereIsAtLeastOneField ? (
        <>
          <div className="grid gap-3 grid-cols-2 mb-sm">
            <label className="block text-gray-600 font-medium mb-xs">Key</label>
            <label className="block text-gray-600 font-medium mb-xs">
              Value
            </label>
          </div>

          {fields.map((field, index) => (
            <KeyValueHeader
              fieldId={field.id}
              fieldName={name}
              rowIndex={index}
              removeRow={remove}
            />
          ))}
        </>
      ) : null}

      <Button
        icon={<FaPlusCircle />}
        onClick={() => {
          append({ name: '', value: '', type: 'from_value' });
        }}
        size="sm"
      >
        Add additional header
      </Button>
    </div>
  );
};
