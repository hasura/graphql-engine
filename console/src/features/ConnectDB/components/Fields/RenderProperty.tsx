import React from 'react';
import { Controller } from 'react-hook-form';
import { Property } from '@/features/DataSource';

import { Switch } from '@/new-components/Switch';
import { InputField, Select } from '@/new-components/Form';
import { Collapse } from '@/new-components/Collapse';
import { Field } from './Fields';

interface RenderPropertyProps {
  name: string;
  property: Property;
  otherSchemas: Record<string, Property>;
}

export const RenderProperty = ({
  property,
  name,
  otherSchemas,
}: RenderPropertyProps) => {
  switch (property.type) {
    case 'string':
      if (property.enum) {
        return (
          <Select
            options={property.enum.map(option => ({
              value: option,
              label: option,
            }))}
            name={name}
            label={property.description ?? name}
          />
        );
      }
      return (
        <InputField
          type="text"
          name={name}
          label={property.description ?? name}
        />
      );
    case 'number':
      return (
        <InputField
          type="number"
          name={name}
          label={property.description ?? name}
        />
      );
    case 'boolean':
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
    case 'object':
      if (property.nullable) {
        return (
          <Collapse rootClassName="bg-white p-6 border border-gray-300 rounded space-y-4 mb-6 max-w-xl">
            <Collapse.Header>
              <span className="text-base text-gray-600 font-semibold">
                {property.description}
              </span>
            </Collapse.Header>
            <Collapse.Content>
              {Object.entries(property.properties).map(
                ([key, _property], i) => (
                  <div key={`${name}.${key}.${i}`}>
                    <Field
                      property={_property}
                      otherSchemas={otherSchemas}
                      name={`${name}.${key}`}
                    />
                  </div>
                )
              )}
            </Collapse.Content>
          </Collapse>
        );
      }
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
    default:
      throw Error('Case not handled');
  }
};
