import React from 'react';
import { useFormContext } from 'react-hook-form';
import { Property, OneOf } from '@/features/DataSource';

import { Field } from './Fields';

import { getProperty, isRef } from '../../DataSource/common/utils';

interface Props {
  name: string;
  property: OneOf;
  otherSchemas: Record<string, Property>;
}

export const RadioGroup = ({ property, otherSchemas, name }: Props) => {
  const { setValue } = useFormContext();

  const [currentOption, setCurrentOption] = React.useState(0);

  const options = React.useMemo(
    () =>
      property.oneOf.map((oneOfProperty, i) => {
        if (isRef(oneOfProperty))
          return (
            getProperty(oneOfProperty, otherSchemas).description ??
            `Option-${i}`
          );
        return oneOfProperty.description ?? `Option-${i}`;
      }),
    [property, otherSchemas]
  );

  const currentProperty =
    currentOption !== undefined ? property.oneOf[currentOption] : undefined;

  return (
    <div className="bg-white p-6 border border-gray-300 rounded space-y-4 mb-6 max-w-xl">
      <div>
        <label className="text-base text-gray-600 font-semibold">
          {property.description}
        </label>
        <p className="leading-5 text-gray-500">Select an option</p>
      </div>
      <fieldset className="mt-4">
        <legend className="sr-only">Notification method</legend>
        <div className="flex items-center space-y-0 space-x-6 text-sm">
          {options.map((option, i) => (
            <div key={option} className="flex items-center">
              <input
                id={option}
                name="notification-method"
                type="radio"
                defaultChecked={i === currentOption}
                className="focus:ring-blue-500 text-blue-60"
                onChange={() => {
                  setCurrentOption(i);
                  setValue(name, undefined);
                }}
              />
              <label
                htmlFor={option}
                className="ml-3 block font-medium text-gray-700"
              >
                {option}
              </label>
            </div>
          ))}
        </div>
      </fieldset>

      {!!currentProperty && (
        <Field
          name={name}
          property={currentProperty}
          otherSchemas={otherSchemas}
        />
      )}
    </div>
  );
};
