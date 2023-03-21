import { Collapsible } from '../../../new-components/Collapsible';
import { OpenApiReference, OpenApiSchema } from '@hasura/dc-api-types';
import { capitalize } from '../../../components/Common/utils/tsUtils';
import React from 'react';
import get from 'lodash/get';
import { ErrorComponentTemplate } from '../../../new-components/Form';
import { FaExclamationCircle } from 'react-icons/fa';
import { useFormContext } from 'react-hook-form';
import { RenderProperty } from './RenderProperty';
import { getInputAttributes } from '../utils';

export const isObjectInputField = (
  configSchema: OpenApiSchema
): configSchema is OpenApiSchema & {
  properties: Record<string, OpenApiSchema | OpenApiReference>;
  type: 'object';
} => {
  const { type, properties } = configSchema;

  /**
   * check if the type is object and it has properties!!
   */
  if (type === 'object' && properties) return true;

  return false;
};

export const ObjectInputField = ({
  name,
  configSchema,
  otherSchemas,
}: {
  name: string;
  configSchema: OpenApiSchema & {
    properties: Record<string, OpenApiSchema | OpenApiReference>;
    type: 'object';
  };
  otherSchemas: Record<string, OpenApiSchema>;
}) => {
  const { label } = getInputAttributes(name, configSchema);

  const isObjectSchemaRequired = configSchema.nullable === false;
  const {
    formState: { errors },
  } = useFormContext();
  const maybeError = get(errors, name);
  return (
    <div>
      <Collapsible
        triggerChildren={
          <span className="font-semibold">
            {capitalize(label)}
            {maybeError ? (
              <span>
                <ErrorComponentTemplate
                  label={
                    <>
                      <FaExclamationCircle className="fill-current h-4 w-4 mr-xs shrink-0" />
                      {Object.entries(maybeError ?? {}).length} Errors found!
                    </>
                  }
                  ariaLabel={
                    `${
                      Object.entries(maybeError ?? {}).length
                    } Errors found!` ?? ''
                  }
                  role="alert"
                />
              </span>
            ) : null}
          </span>
        }
        defaultOpen={isObjectSchemaRequired}
      >
        {Object.entries(configSchema.properties).map(
          ([propertyName, property]) => {
            return (
              <RenderProperty
                name={`${name}.${propertyName}`}
                configSchema={property}
                otherSchemas={otherSchemas}
                key={`${name}.${propertyName}`}
              />
            );
          }
        )}
      </Collapsible>
    </div>
  );
};
