import { OpenApiReference, OpenApiSchema } from '@hasura/dc-api-types';
import React from 'react';
import { getReferenceObject, isReferenceObject } from '../utils';

import { isTextInputField, TextInputField } from './TextInputField';
import { isNumberInputField, NumberInputField } from './NumberInputField';
import { BooleanInputField, isBooleanInputField } from './BooleanInputField';
import {
  FreeFormObjectField,
  isFreeFormObjectField,
} from './FreeFormObjectField';
import { isObjectInputField, ObjectInputField } from './ObjectInputField';
import {
  isTextArrayInputField,
  TextArrayInputField,
} from './TextArrayInputField';
import {
  isObjectArrayInputField,
  ObjectArrayInputField,
} from './ObjectArrayInputField';
import { isOneOf, OneOfInputField } from './OneOfInputField';
import { EnumInputField, isEnumInputField } from './EnumInputField';

export const RenderProperty = ({
  name,
  configSchema,
  otherSchemas,
}: {
  name: string;
  configSchema: OpenApiSchema | OpenApiReference;
  otherSchemas: Record<string, OpenApiSchema>;
}) => {
  /**
   * If it's a reference, find the reference and render the property.
   */
  if (isReferenceObject(configSchema))
    return (
      <RenderProperty
        name={name}
        configSchema={getReferenceObject(configSchema.$ref, otherSchemas)}
        otherSchemas={otherSchemas}
      />
    );

  /**
   * Basic input conditions -> these field are terminal. They do not have any recursive behaviour to them.
   */
  if (isEnumInputField(configSchema))
    return <EnumInputField name={name} configSchema={configSchema} />;

  if (isTextInputField(configSchema))
    return <TextInputField name={name} configSchema={configSchema} />;

  if (isTextArrayInputField(configSchema, otherSchemas))
    return <TextArrayInputField name={name} configSchema={configSchema} />;

  if (isNumberInputField(configSchema, otherSchemas))
    return <NumberInputField name={name} configSchema={configSchema} />;

  if (isBooleanInputField(configSchema))
    return <BooleanInputField name={name} configSchema={configSchema} />;

  if (isFreeFormObjectField(configSchema))
    return <FreeFormObjectField name={name} configSchema={configSchema} />;

  /**
   * Complex input conditions -> these fields are either stacked or recursive.
   */
  if (isObjectInputField(configSchema))
    return (
      <ObjectInputField
        name={name}
        otherSchemas={otherSchemas}
        configSchema={configSchema}
      />
    );

  /**
   * Array of object input
   */
  if (isObjectArrayInputField(configSchema, otherSchemas))
    return (
      <ObjectArrayInputField
        configSchema={configSchema}
        name={name}
        otherSchemas={otherSchemas}
      />
    );

  if (isOneOf(configSchema))
    return (
      <OneOfInputField
        configSchema={configSchema}
        name={name}
        otherSchemas={otherSchemas}
      />
    );

  return <div>No Input Map ({name}) </div>;
};
