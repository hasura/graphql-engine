import React, { ReactText } from 'react';
import { useFieldArray } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import { Button } from '../Button';
import { RequestHeadersSelectorSchema } from './schema';
import { KeyValueHeader } from './components/KeyValueHeader';

export interface RequestHeadersSelectorProps {
  name: string;
  addButtonText?: ReactText;
  typeSelect?: boolean;
}

export const RequestHeadersSelector = (props: RequestHeadersSelectorProps) => {
  const { name, addButtonText = 'Add', typeSelect = true } = props;
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
              key={field.id}
              fieldId={field.id}
              fieldName={name}
              rowIndex={index}
              typeSelect={typeSelect}
              removeRow={remove}
            />
          ))}
        </>
      ) : null}

      <Button
        data-testid="add-header"
        icon={<FaPlusCircle />}
        onClick={() => {
          append({
            name: '',
            value: '',
            ...(typeSelect && { type: 'from_value' }),
          });
        }}
        size="sm"
      >
        {addButtonText}
      </Button>
    </div>
  );
};
