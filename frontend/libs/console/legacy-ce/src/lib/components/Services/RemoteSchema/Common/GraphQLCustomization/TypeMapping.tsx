import React, { useEffect, useState } from 'react';
import { FaTimes } from 'react-icons/fa';
import { inputStyles } from '../../constants';

type TypeMap = { type: string; custom_name: string };

type Props = {
  types: string[];
  typeMappings: TypeMap[];
  onChange: (updatedMaps: TypeMap[]) => void;
  label?: string;
};

const SelectOne = ({
  options,
  value,
  onChange,
  label,
}: {
  options: Props['types'];
  value: string;
  onChange: (e: any) => void;
  label?: string;
}) => (
  <select
    value={value}
    onChange={onChange}
    className={`${inputStyles} font-normal`}
    data-test={`remote-schema-customization-${label}-lhs-input`}
    name={`${label}-lhs`}
  >
    <option value="" className="text-base">
      Select Type ...
    </option>
    {[...options].sort().map((op, i) => (
      <option value={op} key={i}>
        {op}
      </option>
    ))}
  </select>
);

const TypeMapping = ({ types, typeMappings, onChange, label }: Props) => {
  const [existingMaps, updateExistingMaps] = useState(typeMappings);
  const [newMap, setNewMap] = useState<TypeMap>({ type: '', custom_name: '' });

  const onModifyItem = (index: number, newVal: TypeMap) => {
    const updatedMaps = existingMaps;
    updatedMaps[index] = newVal;
    onChange(updatedMaps);
  };

  const onAddItem = (inputMap: TypeMap) => {
    onChange([...existingMaps, inputMap]);
    setNewMap({ type: '', custom_name: '' });
  };

  const onDeleteItem = (index: number) => {
    onChange(existingMaps.filter((t, i) => i !== index));
  };

  useEffect(() => {
    updateExistingMaps(typeMappings);
  }, [typeMappings]);

  return (
    <>
      {existingMaps.map(({ type, custom_name }, i) => {
        return (
          <div className="flex items-center mt-md" key={i}>
            <label className="w-2/3">
              <SelectOne
                options={[
                  ...types.filter(
                    t => !existingMaps.map(x => x.type).includes(t)
                  ),
                  type,
                ]}
                value={type}
                key={i}
                onChange={e => {
                  onModifyItem(i, {
                    type: e.target.value,
                    custom_name,
                  });
                }}
                label={`${label ?? 'no-value'}-${i}`}
              />
            </label>

            <span className="pl-md pr-md"> : </span>
            <div className="w-2/3">
              <input
                type="text"
                value={custom_name}
                className={`${inputStyles} font-normal`}
                onChange={e => {
                  onModifyItem(i, { type, custom_name: e.target.value });
                }}
                data-test={`remote-schema-customization-${
                  label ?? 'no-value'
                }-${i}-rhs-input`}
                name={`${label}-rhs[${i}]`}
              />
            </div>
            <div>
              <FaTimes
                className="ml-2.5 w-4 min-w-4 cursor-pointer h-md w-md"
                onClick={() => onDeleteItem(i)}
              />
            </div>
          </div>
        );
      })}
      <div className="flex items-center mt-md" style={{ maxWidth: '96%' }}>
        <label className="w-2/3">
          <SelectOne
            options={types.filter(
              t => !existingMaps.map(x => x.type).includes(t)
            )}
            label={`${label ?? 'no-value'}`}
            value={newMap.type}
            onChange={e => {
              setNewMap({ ...newMap, type: e.target.value });
              onAddItem({ ...newMap, type: e.target.value });
            }}
          />
        </label>
        <span className="pl-md pr-md"> : </span>
        <div className="w-2/3">
          <input
            type="text"
            value={newMap.custom_name}
            className={`${inputStyles} font-normal`}
            data-test={`remote-schema-customization-${
              label ?? 'no-value'
            }-rhs-input`}
            onChange={e =>
              setNewMap({ ...newMap, custom_name: e.target.value })
            }
            name={`${label}-rhs`}
            // onBlur={() => {
            //   onAddItem(newMap);
            // }}
          />
        </div>
      </div>
    </>
  );
};

export default TypeMapping;
