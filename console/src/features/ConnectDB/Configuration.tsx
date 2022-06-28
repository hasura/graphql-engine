/* eslint-disable no-underscore-dangle */
import React, { useState } from 'react';
import { Controller, useFormContext } from 'react-hook-form';
import {
  Ref,
  Property,
  DataSource,
  SupportedDrivers,
  Feature,
  OneOf,
} from '@/features/DataSource';
import get from 'lodash.get';
import axios from 'axios';
import { Switch } from '@/new-components/Switch';
import { InputField, Select } from '@/new-components/Form';
import { useQuery } from 'react-query';
import {
  getPropertyByRef,
  isOneOf,
  isProperty,
  isRef,
} from '../DataSource/utils';

const useConfigSchema = (driver: SupportedDrivers) => {
  const fetch = axios.create();
  return useQuery({
    queryKey: [driver, 'configSchema'],
    queryFn: async () => {
      return DataSource(fetch).connectDB.getConfigSchema(driver);
    },
    enabled: !!driver,
  });
};

const RadioGroup = ({
  property,
  otherSchemas,
  name,
}: {
  name: string;
  property: OneOf;
  otherSchemas: Record<string, Property>;
}) => {
  const { setValue } = useFormContext();

  // const currentOption: number = watch(`extra_props.${name}.radio_group_option`);

  const options = property.oneOf.map((_property, i) => {
    if (isRef(_property))
      return (
        getPropertyByRef(_property, otherSchemas).description ?? `Option-${i}`
      );

    return _property.description ?? `Option-${i}`;
  });

  const [currentOption, setCurrentOption] = useState<undefined | number>();

  const currentProperty =
    currentOption !== undefined ? property.oneOf[currentOption] : undefined;

  return (
    <>
      <div className="my-8 rounded border bg-white border-gray-300 p-4">
        <div className="text-gray-600 font-semibold py-3">
          {property.description}
        </div>
        {options.map((option, i) => {
          return (
            <span key={i} className="mr-md">
              <input
                type="radio"
                onChange={() => {
                  setCurrentOption(i);
                  setValue(name, undefined);
                }}
                name={`${name}-select`}
                className="radio radio-primary"
                value={i}
              />
              <label className="ml-sm">{option}</label>
            </span>
          );
        })}

        <div>
          {currentProperty ? (
            <div className="grid card bg-base-300 rounded-box my-4">
              <Field
                property={currentProperty}
                otherSchemas={otherSchemas}
                name={name}
              />
            </div>
          ) : (
            <div className="my-3 italic">Select an option</div>
          )}
        </div>
      </div>
    </>
  );
};

const Field = ({
  name,
  property,
  otherSchemas,
}: {
  name: string;
  property: Ref | Property | { oneOf: (Property | Ref)[] };
  otherSchemas: Record<string, Property>;
}) => {
  if (isProperty(property)) {
    if (property.type === 'string') {
      if (property.enum) {
        return (
          <div className="my-4">
            <Select
              options={property.enum.map(option => ({
                value: option,
                label: option,
              }))}
              name={name}
              label={property.description ?? name}
            />
          </div>
        );
      }
      return (
        <InputField
          type="text"
          name={name}
          label={property.description ?? name}
          className="my-4"
        />
      );
    }

    if (property.type === 'number')
      return (
        <InputField
          type="number"
          name={name}
          label={property.description ?? name}
          className="my-4"
        />
      );

    if (property.type === 'boolean')
      return (
        <div className="max-w-xl flex justify-between my-4">
          <label className="font-semibold text-gray-600">
            {property.description ?? name}
          </label>
          <Controller
            name={name}
            render={({ field: { onChange, value } }) => (
              <Switch checked={value} onCheckedChange={onChange} />
            )}
          />
        </div>
      );

    if (property.type === 'object')
      return (
        <div>
          {Object.entries(property.properties).map(([key, _property], i) => {
            return (
              <div key={`${name}.${key}.${i}`}>
                <Field
                  property={_property}
                  otherSchemas={otherSchemas}
                  name={`${name}.${key}`}
                />
              </div>
            );
          })}
        </div>
      );
  }

  if (isRef(property)) {
    const ref = property.$ref;
    const _property = get(otherSchemas, ref.split('/').slice(2).join('.'));

    return (
      <Field property={_property} otherSchemas={otherSchemas} name={name} />
    );
  }

  if (isOneOf(property)) {
    return (
      <RadioGroup property={property} otherSchemas={otherSchemas} name={name} />
    );
  }

  return null;
};

export const Configuration = ({ name }: { name: string }) => {
  const { watch } = useFormContext();
  const driver: SupportedDrivers = watch('driver');
  const { data: schema, isLoading, isError } = useConfigSchema(driver);

  if (schema === Feature.NotImplemented)
    return <>Feature is not available for this {driver}</>;

  if (!driver) return <>Driver not selected</>;

  if (isLoading) return <>Loading configuration info...</>;

  if (isError) return <>welp something broke ! </>;

  if (!schema) return <>Unable to find any schema for the {driver}</>;

  if (schema.configSchema.type !== 'object') return null;

  return (
    <div>
      {Object.entries(schema.configSchema.properties).map(([key, value], i) => {
        return (
          <Field
            property={value}
            otherSchemas={schema.otherSchemas}
            key={i}
            name={`${name}.${key}`}
          />
        );
      })}
    </div>
  );
};
