import { OpenApiReference, OpenApiSchema } from '@hasura/dc-api-types';
import get from 'lodash/get';
import { useFormContext } from 'react-hook-form';
import { RenderProperty } from './RenderProperty';
import { getInputAttributes } from '../utils';
import { capitalize } from 'inflection';
import { Collapsible } from '../../Collapsible';
import { ErrorMessage } from '../../Form';

export const isObjectInputField = (
  configSchema: OpenApiSchema,
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
                <ErrorMessage
                  error={`${Object.entries(maybeError ?? {}).length} Errors found!`}
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
          },
        )}
      </Collapsible>
    </div>
  );
};
