import { OpenApiReference, OpenApiSchema } from '@hasura/dc-api-types';
import React, { useState } from 'react';
import { useFormContext } from 'react-hook-form';
import { ZodSchema } from 'zod';
import {
  getInputAttributes,
  getReferenceObject,
  isReferenceObject,
  transformSchemaToZodObject,
} from '../utils';
import { RenderProperty } from './RenderProperty';

type OneOfSchema = OpenApiSchema & {
  oneOf: Array<OpenApiSchema | OpenApiReference>;
};

export const isOneOf = (
  configSchema: OpenApiSchema
): configSchema is OneOfSchema => {
  return Object.keys(configSchema).includes('oneOf');
};

const getUnionSchemas = (
  configSchema: OneOfSchema,
  references: Record<string, OpenApiSchema>
) => {
  const schemas = configSchema.oneOf.map(oneOfProperty => {
    const oneOfPropertySchema = isReferenceObject(oneOfProperty)
      ? getReferenceObject(oneOfProperty.$ref, references)
      : oneOfProperty;

    const zodSchema = transformSchemaToZodObject(
      oneOfPropertySchema,
      references
    );

    return zodSchema;
  });
  return schemas;
};

function getOption(value: any, zodSchemas: ZodSchema[]) {
  if (!value) return undefined;

  const passingSchema = zodSchemas.findIndex(zodSchema => {
    try {
      return zodSchema.parse(value);
    } catch {
      return false;
    }
  });

  return passingSchema;
}

const getOptions = (
  configSchema: OneOfSchema,
  otherSchemas: Record<string, OpenApiSchema>
): string[] => {
  const options = configSchema.oneOf.map((oneOfProperty, i) => {
    const oneOfPropertyDef = isReferenceObject(oneOfProperty)
      ? getReferenceObject(oneOfProperty.$ref, otherSchemas)
      : oneOfProperty;

    return oneOfPropertyDef.title ?? `Option ${i + 1}`;
  });

  return options;
};

export const OneOfInputField = ({
  name,
  configSchema,
  otherSchemas,
}: {
  name: string;
  configSchema: OneOfSchema;
  otherSchemas: Record<string, OpenApiSchema>;
}) => {
  const { label } = getInputAttributes(name, configSchema);

  const { watch, setValue } = useFormContext<Record<string, any>>();

  const value = watch(name);

  const zodSchemas = getUnionSchemas(configSchema, otherSchemas);

  const options = getOptions(configSchema, otherSchemas);

  const [selectedOptionIndex, setSelectedOptionIndex] = useState<number>(
    getOption(value, zodSchemas) ?? 0
  );

  const currentRenderedProperty =
    selectedOptionIndex !== undefined
      ? configSchema.oneOf[selectedOptionIndex]
      : null;

  return (
    <div className="bg-white p-6 border border-gray-300 rounded space-y-4 mb-6 max-w-xl">
      <div>
        <label className="text-base text-gray-600 font-semibold mb-0">
          {label}
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
                defaultChecked={i === selectedOptionIndex}
                className="focus:ring-blue-500 text-blue-60 mt-0"
                onChange={() => {
                  setSelectedOptionIndex(i);
                  setValue(name, undefined);
                }}
              />
              <label
                htmlFor={option}
                className="ml-3 block font-medium text-gray-700 mb-0"
              >
                {option}
              </label>
            </div>
          ))}
        </div>
      </fieldset>

      {currentRenderedProperty && selectedOptionIndex !== undefined ? (
        <RenderProperty
          name={name}
          configSchema={currentRenderedProperty}
          otherSchemas={otherSchemas}
          key={`${name}.${selectedOptionIndex}`}
        />
      ) : null}
    </div>
  );
};
