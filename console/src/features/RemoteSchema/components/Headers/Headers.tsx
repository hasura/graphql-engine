import { Button } from '@/new-components/Button';
import React from 'react';
import { useFieldArray, useFormContext } from 'react-hook-form';
import { FaPlusCircle } from 'react-icons/fa';
import { RiCloseCircleFill } from 'react-icons/ri';
import { THeaders } from './schema';

export const Headers = ({ name }: { name: string }) => {
  const { register } = useFormContext();

  const { fields, append, remove } = useFieldArray<Record<string, THeaders>>({
    name,
  });

  return (
    <div>
      {fields.length ? (
        <div className="grid gap-3 grid-cols-2 mb-sm">
          <label
            htmlFor="table_name"
            className="block text-gray-600 font-medium mb-xs"
          >
            Key
          </label>

          <label
            htmlFor="table_name"
            className="block text-gray-600 font-medium mb-xs"
          >
            Value
          </label>
        </div>
      ) : null}

      {fields.map((field, index) => {
        return (
          <div className="grid gap-4 grid-cols-2 mb-sm" key={field.id}>
            <input
              type="text"
              className="w-full block h-10 shadow-sm rounded border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
              placeholder="Key..."
              {...register(`${name}[${index}].name`)}
            />

            <div className="flex shadow-sm rounded">
              <select
                {...register(`${name}[${index}].type`)}
                className="inline-flex rounded-l border border-r-0 border-gray-300 bg-white hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
              >
                <option value="from_value">Value</option>
                <option value="from_env">Env Var</option>
              </select>
              <input
                {...register(`${name}[${index}].value`)}
                type="text"
                className="flex-1 min-w-0 block w-full px-3 py-2 rounded-r border-gray-300 hover:border-gray-400 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
                placeholder="user-id"
              />
              <div className="col-span-1 flex items-center justify-center pl-1.5">
                <RiCloseCircleFill
                  onClick={() => {
                    remove(index);
                  }}
                  className="cursor-pointer"
                />
              </div>
            </div>
          </div>
        );
      })}
      <Button
        data-testid={`${name}_add_new_row`}
        icon={<FaPlusCircle />}
        onClick={() => {
          append({ name: '', value: '', type: 'from_value' });
        }}
        size="sm"
      >
        Add additonal header
      </Button>
    </div>
  );
};
