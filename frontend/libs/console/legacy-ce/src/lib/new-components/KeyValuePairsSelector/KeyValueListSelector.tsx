import React from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { Button } from '../Button';
import { FaMinusCircle, FaPlusCircle } from 'react-icons/fa';

interface KeyValueListSelectorProps {
  addLabel?: string;
  name: string;
}

export const KeyValueListSelector = (props: KeyValueListSelectorProps) => {
  const { addLabel, name } = props;

  const { register, control } = useFormContext();

  const { fields, append, remove } = useFieldArray({
    control,
    name,
  });

  const addRow = () => {
    append({ key: '', value: '', checked: false });
  };

  return (
    <div>
      {fields.map((field: any, index) => (
        <div key={field.id} className="flex items-center space-x-4 mb-2">
          <input
            type="checkbox"
            {...register(`${name}.${index}.checked`)}
            className="text-blue-600 rounded"
          />
          <input
            type="text"
            {...register(`${name}.${index}.key`)}
            defaultValue={field.key}
            className="border border-gray-300 p-2 rounded flex-1"
            placeholder="Key"
          />
          <input
            key={`${name}-${field.key}-value`}
            type="text"
            {...register(`${name}.${index}.value`)}
            defaultValue={field.value}
            className="border border-gray-300 p-2 rounded flex-1"
            placeholder="Value"
          />
          <div className="w-4">
            {index > 0 && (
              <FaMinusCircle
                onClick={() => remove(index)}
                className="cursor-pointer"
              />
            )}
          </div>
        </div>
      ))}
      <Button
        onClick={addRow}
        icon={<FaPlusCircle />}
        size="sm"
        className="mt-1"
      >
        {addLabel || 'Add'}
      </Button>
    </div>
  );
};

export default KeyValueListSelector;
