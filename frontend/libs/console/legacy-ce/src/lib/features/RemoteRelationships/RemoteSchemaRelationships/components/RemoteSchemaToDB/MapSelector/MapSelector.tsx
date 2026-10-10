import { useState } from 'react';
import { GiPlainCircle, GiCircle } from 'react-icons/gi';
import { FiArrowRight } from 'react-icons/fi';
import { TiDelete } from 'react-icons/ti';
import { useFormContext } from 'react-hook-form';
import { FaColumns, FaFont } from 'react-icons/fa';
import {
  Card,
  createTextOption,
  IconButton,
  ReactSelect,
  SelectField,
  Text,
} from '@hasura/shared/ui';
import { Flex } from '@radix-ui/themes';
import { Schema } from '../schema';
import { createFilter } from 'react-select';

export type TypeMap = { field: string; column: string };

const filterOption = createFilter({
  ignoreCase: true,
  matchFrom: 'any',
});

type MapSelectorProps = {
  types: string[];
  columns: string[];
  mapping: TypeMap[];
  setMapping: (values: TypeMap[]) => void;
};

const relationshipTypeOptions = [
  {
    label: 'Array Relationship',
    value: 'array',
  },
  {
    label: 'Object Relationship',
    value: 'object',
  },
];

const name = 'mapping';

export const MapSelector = ({ types, columns }: MapSelectorProps) => {
  const [newMap, setNewMap] = useState<TypeMap>({ field: '', column: '' });

  const { setValue, watch } = useFormContext<Schema>();

  const typeMappings = watch(name);

  const onModifyItem = (index: number, newVal: TypeMap) => {
    const updatedMaps = [...typeMappings];
    updatedMaps[index] = newVal;
    setValue(name, updatedMaps);
  };

  const onAddItem = (inputMap: TypeMap) => {
    setValue(name, [...typeMappings, inputMap]);
    setNewMap({ field: '', column: '' });
  };

  const onDeleteItem = (index: number) => {
    setValue(
      name,
      typeMappings.filter((t, i) => i !== index),
    );
  };

  const onNewMapChange = (newValue: TypeMap) => {
    if (newValue.column && newValue.field) {
      onAddItem(newValue);
    } else {
      setNewMap(newValue);
    }
  };

  const typeMappingsOptions = types
    .filter((t) => !typeMappings.map((x) => x.field).includes(t))
    .map(createTextOption);
  const columnOptions = columns.map(createTextOption);

  return (
    <Card>
      <div className="w-full sm:w-5/12 mb-4">
        <div className="mb-4">
          <SelectField
            name="relationshipType"
            label="Type"
            dataTest="select-rel-type"
            placeholder="Select a relationship type..."
            options={relationshipTypeOptions}
            noErrorPlaceholder
          />
        </div>
      </div>
      <div className="grid grid-cols-12 gap-3 mb-1 text-muted font-semibold">
        <Flex className="col-span-5" align="center" gap="1">
          <Text color="green">
            <GiPlainCircle />
          </Text>
          <FaFont className="text-sm" />
          <Text weight="bold">Source Field</Text>
        </Flex>
        <div className="col-span-1 text-center" />
        <Flex className="col-span-5" align="center" gap="1">
          <Text color="green">
            <GiCircle className="text-sm mr-1" style={{ color: '#6366f1' }} />
          </Text>
          <FaColumns className="text-sm mr-1" />
          <Text weight="bold">Reference Column</Text>
        </Flex>
        <div className="col-span-1 text-right" />
      </div>
      {typeMappings.map(({ field, column }, i) => {
        return (
          <div className="grid grid-cols-12 gap-3 mb-2" key={i}>
            <div className="col-span-5">
              <ReactSelect
                options={typeMappingsOptions}
                filterOption={filterOption}
                value={createTextOption(field)}
                placeholder="Select a field..."
                aria-label="Source field"
                key={i}
                isSearchable
                onChange={(option) => {
                  onModifyItem(i, {
                    field: option?.value ?? '',
                    column,
                  });
                }}
              />
            </div>

            <Flex className="col-span-1" align="center" justify="center">
              <FiArrowRight />
            </Flex>
            <div className="col-span-5">
              <ReactSelect
                options={columnOptions}
                value={createTextOption(column)}
                placeholder="Select a column..."
                aria-label="Reference column"
                key={i}
                filterOption={filterOption}
                isSearchable
                onChange={(option) => {
                  if (!option) {
                    return;
                  }
                  onModifyItem(i, {
                    field,
                    column: option.value,
                  });
                }}
              />
            </div>
            <Flex className="col-span-1" align="center" justify="center">
              <IconButton color="indigo" variant="ghost" radius="full">
                <TiDelete
                  className="w-6 h-6"
                  data-test={`remove-type-map-${i}`}
                  onClick={() => onDeleteItem(i)}
                />
              </IconButton>
            </Flex>
          </div>
        );
      })}
      <div className="grid grid-cols-12 gap-3 mb-2">
        <div className="col-span-5">
          <ReactSelect
            options={typeMappingsOptions}
            value={createTextOption(newMap.field)}
            filterOption={filterOption}
            isSearchable
            placeholder="Select a field..."
            aria-label="Source field"
            onChange={(option) => {
              const newValue = { ...newMap, field: option?.value ?? '' };
              onNewMapChange(newValue);
            }}
          />
        </div>
        <Flex align="center" justify="center" className="col-span-1">
          <FiArrowRight />
        </Flex>
        <div className="col-span-5">
          <ReactSelect
            options={columnOptions}
            value={createTextOption(newMap.column)}
            placeholder="Select a column..."
            aria-label="Reference column"
            filterOption={filterOption}
            isSearchable
            onChange={(option) => {
              const newValue = { ...newMap, column: option?.value ?? '' };
              onNewMapChange(newValue);
            }}
          />
        </div>
      </div>
    </Card>
  );
};
