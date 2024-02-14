import React, { useEffect, useState } from 'react';
import { GiPlainCircle, GiCircle } from 'react-icons/gi';
import { FiArrowRight } from 'react-icons/fi';
import { TiDelete } from 'react-icons/ti';
// eslint-disable-next-line no-restricted-imports
import { useFormContext } from 'react-hook-form';
import { FaColumns, FaFont } from 'react-icons/fa';
import { Select } from '../Form';

export type TypeMap = { field: string; column: string };

type MapSelectorProps = {
  types: string[];
  columns: string[];
  typeMappings: TypeMap[];
  onChange: (updatedMaps: TypeMap[]) => void;
  placeholder: string;
  name: string;
};

const SelectOne = ({
  options,
  value,
  placeholder,
  dataTest,
  onChange,
}: {
  options: MapSelectorProps['types'];
  value: string;
  placeholder: string;
  dataTest?: string;
  onChange: (e: any) => void;
}) => (
  <select
    value={value}
    onChange={onChange}
    data-test={dataTest}
    className="block w-full h-input font-normal shadow-sm rounded border border-gray-300 hover:border-gray-400 focus:outline-0 focus:ring-2 focus:ring-yellow-200 focus:border-yellow-400"
  >
    <option disabled value="">
      {placeholder}
    </option>
    {options.map((op, i) => (
      <option value={op} key={i}>
        {op}
      </option>
    ))}
  </select>
);

export const MapSelector = ({
  types,
  typeMappings,
  onChange,
  name,
  columns,
}: MapSelectorProps) => {
  const [existingMaps, updateExistingMaps] = useState(typeMappings);
  const [newMap, setNewMap] = useState<TypeMap>({ field: '', column: '' });

  const { setValue } = useFormContext();

  const onModifyItem = (index: number, newVal: TypeMap) => {
    const updatedMaps = existingMaps;
    updatedMaps[index] = newVal;
    onChange(updatedMaps);
  };

  const onAddItem = (inputMap: TypeMap) => {
    onChange([...existingMaps, inputMap]);
    setNewMap({ field: '', column: '' });
  };

  const onDeleteItem = (index: number) => {
    onChange(existingMaps.filter((t, i) => i !== index));
  };

  useEffect(() => {
    updateExistingMaps(typeMappings);
    setValue(name, typeMappings);
  }, [typeMappings]);

  return (
    <div
      id="reference"
      className="rounded bg-gray-50 border border-gray-300 p-md mb-md"
    >
      <div className="w-full sm:w-5/12 mb-md pr-2">
        <div className="mb-md">
          <Select
            name="relationshipType"
            label="Type"
            dataTest="select-rel-type"
            placeholder="Select a relationship type..."
            options={[
              {
                label: 'Array Relationship',
                value: 'array',
              },
              {
                label: 'Object Relationship',
                value: 'object',
              },
            ]}
          />
        </div>
      </div>
      <div className="grid grid-cols-12 gap-3 mb-xs text-muted font-semibold">
        <div className="col-span-5">
          <GiPlainCircle className="mr-1.5 text-green-700" />
          <FaFont className="text-sm mr-xs" />
          Source Field
        </div>
        <div className="col-span-1 text-center" />
        <div className="col-span-5">
          <GiCircle className="text-sm mr-xs" style={{ color: '#6366f1' }} />
          <FaColumns className="text-sm mr-xs" />
          Reference Column
        </div>
        <div className="col-span-1 text-right" />
      </div>
      {existingMaps.map(({ field, column }, i) => {
        return (
          <div
            className="grid grid-cols-12 gap-3 mb-sm text-muted font-semibold"
            key={i}
          >
            <div className="col-span-5">
              <SelectOne
                options={[
                  ...types.filter(
                    t => !existingMaps.map(x => x.field).includes(t)
                  ),
                  field,
                ]}
                value={field}
                placeholder="Select a field..."
                key={i}
                dataTest="select-source-field"
                onChange={e => {
                  onModifyItem(i, {
                    field: e.target.value,
                    column,
                  });
                }}
              />
            </div>

            <div className="col-span-1 flex items-center justify-center">
              <FiArrowRight />
            </div>
            <div className="col-span-5">
              <SelectOne
                options={columns}
                value={column}
                placeholder="Select a column..."
                dataTest="select-ref-col"
                key={i}
                onChange={e => {
                  onModifyItem(i, {
                    field,
                    column: e.target.value,
                  });
                }}
              />
            </div>
            <div className="col-span-1 flex items-center justify-center">
              <TiDelete
                data-test={`remove-type-map-${i}`}
                onClick={() => onDeleteItem(i)}
              />
            </div>
          </div>
        );
      })}
      <div className="grid grid-cols-12 gap-3 mb-sm text-muted font-semibold">
        <div className="col-span-5">
          <SelectOne
            options={types.filter(
              t => !existingMaps.map(x => x.field).includes(t)
            )}
            value={newMap.field}
            dataTest="select-source-field"
            placeholder="Select a field..."
            onChange={e => {
              setNewMap({ ...newMap, field: e.target.value });
              onAddItem({ ...newMap, field: e.target.value });
            }}
          />
        </div>
        <div className="col-span-1 flex items-center justify-center">
          <FiArrowRight />
        </div>
        <div className="col-span-5">
          <SelectOne
            options={columns}
            value={newMap.column}
            placeholder="Select a column..."
            onChange={e => {
              setNewMap({ ...newMap, column: e.target.value });
            }}
          />
        </div>
      </div>
    </div>
  );
};
